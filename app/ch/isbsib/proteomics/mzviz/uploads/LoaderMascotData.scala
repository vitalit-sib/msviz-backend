package ch.isbsib.proteomics.mzviz.uploads

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.controllers.experimental.ExperimentalController._
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, ExpMs1Spectrum}
import ch.isbsib.proteomics.mzviz.experimental.{MSRun, RunId}
import ch.isbsib.proteomics.mzviz.experimental.importer.{LoaderMGF, LoaderMzML, LoaderMzXML}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.{SearchInfoDBService, ProteinMatchMongoDBService, MatchMongoDBService}
import ch.isbsib.proteomics.mzviz.uploads.LoaderMQData._
import play.api.libs.json.Json
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB

import scala.concurrent.Future
import java.nio.file.{Paths, Files}
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File

import scala.util.{Failure, Success}
import scala.xml.Elem

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMascotData(val db: DefaultDB) {

  // file types that are required
  val requiredTypes = Set("mzid", "mgf", "mzml")

  // define services
  val ms1Service = new ExpMs1BinMongoDBService(db)
  val msnService = new ExpMongoDBService(db)
  val matchService = new MatchMongoDBService(db)
  val protMatchService = new ProteinMatchMongoDBService(db)
  val searchInfoService = new SearchInfoDBService(db)

  /**
   * load a zip file containing a run per subfolder
   *
   * @param zipPath
   * @return
   */
  def loadZip(zipPath: String, intensityThreshold: Double): Future[Int] = {

    val unzipPath = FileFinder.getHighestDir(Unzip.unzip(new File(zipPath)))
    loadUnzipped(unzipPath, intensityThreshold)
  }


  def loadUnzipped(path: String, intensityThreshold: Double): Future[Int] = {

    // get the list of files in the directory
    val fileList = FileFinder.getListOfFiles(path)

    // keep mzId files
    val mzIdFiles = fileList.filter(x => x.getName.split("\\.").last.toLowerCase() == "mzid")

    // list of searchIds
    val searchIds = mzIdFiles.map(x => SearchId(x.getName.split("\\.")(0)))

    // get the XML parsers
    val elemList = mzIdFiles.map(file => scala.xml.XML.loadFile(file))

    // get the corresponding mzML files
    // zip XML elements and mzid names, List[(Elem, File)]
    val mzMlFiles = elemList.zip(mzIdFiles).map({
      tuple =>
        val found= LoaderMzIdent.parseSpectraFilename(tuple._1)
        var filename= path + "/" + found

        //if resubmitted job, SpectraData location field is empty, so we take mzid name
        if (found == ""){
          filename= tuple._2.toString.split("\\.")(0)
        }
        new File(filename + ".mzML")
    })

    // assert that all mzML files are here
    mzMlFiles.foreach({ mzMlFile =>
      if (! Files.exists(mzMlFile.toPath)) {
        throw new RuntimeException("[" + mzMlFile + "] not found. If resubmitted job please rename your mzML files.")
      }
    })

    // assert that searchIds are not already inserted
    searchIds.foreach({ id =>
      for{
        found <- searchInfoService.isSearchIdExist(id)
      } yield {
        if(found) throw new RuntimeException("[" + id + "] was already inserted. Please delete this entry before re-insertion")
      }
    })

    for{
      nrExp <- insertExpData(mzMlFiles.zip(searchIds), intensityThreshold)
      nrMatch <- insertMatchData(mzIdFiles.zip(searchIds.zip(elemList)))
    } yield {
      nrExp + nrMatch
    }



  }


  def insertMatchData(mzIdFiles: List[(File, (SearchId, Elem))]): Future[Int] = {
    // insert one by one
    mzIdFiles.foldLeft(Future{0})( (futureA, b) =>
      for {
        a <- futureA
        c <- insertOneMatch(b._1, b._2)
      } yield {
        a + c
      })

  }

  def insertOneMatch(mzIdFile: File, searchId: (SearchId, Elem)): Future[Int] = {
    val matchData = LoaderMzIdent.parseWithXmlElem(mzIdFile, searchId._1, RunId(searchId._1.value), searchId._2)

    for{
      // and only last the other data
      matchNr <- matchService.insert(matchData._1)
      psmNumber <- protMatchService.insert(matchData._2)
      searchOk <- searchInfoService.insert(matchData._3)

    }yield{
      matchNr + psmNumber + (if(searchOk) 1 else 0)
    }
  }


  def insertExpData(mzMlFiles: List[(File, SearchId)], intensityThreshold: Double): Future[Int] = {
    // insert one by one
    mzMlFiles.foldLeft(Future{0})( (futureA, b) =>
      for {
        a <- futureA
        c <- insertOneExp(b._1, b._2, intensityThreshold)
      } yield {
        a + c
      })
  }


  def insertOneExp(mzMlFile: File, id: SearchId, intensityThreshold: Double): Future[Int] = {

    val itMs1Ms2 = LoaderMzML().parse(mzMlFile, RunId(id.value)).partition(_.isLeft)
    val itMs1: Iterator[ExpMs1Spectrum] = itMs1Ms2._1.map(_.left.get)
    val itMs2: Iterator[ExpMSnSpectrum] = itMs1Ms2._2.map(_.right.get)

    for {
    //Load MS1
      ms1 <- ms1Service.insertMs1spectra(itMs1, intensityThreshold)
      //Load MS2
      ms2 <- msnService.insertMs2spectra(itMs2, RunId(id.value))
    }yield{
      if(ms1) 1 else 0 + ms2
    }
  }

  /**
   * parse a subfolder containing *.mzIdentML, *.mzML, *.mgf
   *
   * @param runPath
   * @return
   */
  def insertRunFromPath(runPath: File, intensityThreshold: Double):Future[Int] = {

    // get the runId
    val runId: RunId = getRunIdFromPath(runPath)

    // check if all required files are here (it is not case sensitive)
    val availableFiles = getRequiredFiles(requiredTypes, runPath)

    // now we insert all the data
    val ms1Iterator = LoaderMzML().parse(availableFiles.get("mzml").get, runId).filter(_.isLeft).map(_.left.get)
    val ms2Iterator = LoaderMGF.load(availableFiles.get("mgf").get, runId)
    val matchData = LoaderMzIdent.parse(availableFiles.get("mzid").get, SearchId(runId.value), runId)

    for{
      //first we insert MS data, because they're slow
      ms1Nr <- ms1Service.insertMs1spectra(ms1Iterator, intensityThreshold)
      ms2Nr <- msnService.insertMs2spectra(ms2Iterator, runId)

      // and only last the other data
      matchNr <- matchService.insert(matchData._1)
      psmNumber <- protMatchService.insert(matchData._2)
      searchOk <- searchInfoService.insert(matchData._3)

    }yield{
      if(ms1Nr) 1 else 0 + ms2Nr + matchNr + psmNumber + (if(searchOk) 1 else 0)
    }

  }


  /**
   * get the required files as a hashmap
   *
   * @param requiredTypes
   * @param runPath
   */
  def getRequiredFiles(requiredTypes: Set[String], runPath: File): Map[String, File] = {

    val availableFiles: List[File] = FileFinder.getListOfFiles(runPath.getAbsolutePath)

    // get file extensions as lower case
    val extensions: List[String] =  availableFiles.map(x =>  x.toString.substring(x.toString.lastIndexOf(".") + 1).toLowerCase)

    val extFilePairs = extensions.zip(availableFiles)

    // find the pair for every file type
    requiredTypes.map({ x =>
      val l = extFilePairs.filter(_._1 == x)
      val hit = if(l.size > 0) l(0) else throw new RuntimeException("Required file type is missing: " + x + " in [" + runPath.getAbsolutePath + "]")
      (x, hit._2)
    }).toMap

  }


  /**
   * get the runId from the directory name
   *
   * @param runPath
   * @return
   */
  def getRunIdFromPath(runPath: File): RunId = {
    val lastPart = runPath.getAbsolutePath.split("\\/").last
    RunId(lastPart)
  }


}




/**
 * the companion object
 */

object LoaderMascotData extends Controller with MongoController {

  val default = new LoaderMascotData(db)

  def apply() = default

}
