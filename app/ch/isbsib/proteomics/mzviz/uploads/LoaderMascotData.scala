package ch.isbsib.proteomics.mzviz.uploads

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.controllers.experimental.ExperimentalController._
import ch.isbsib.proteomics.mzviz.experimental.{MSRun, RunId}
import ch.isbsib.proteomics.mzviz.experimental.importer.{LoaderMGF, LoaderMzML, LoaderMzXML}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.{SearchInfoDBService, ProteinMatchMongoDBService, MatchMongoDBService}
import play.api.libs.json.Json

import scala.concurrent.Future
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File

import scala.util.{Failure, Success}

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMascotData {

  /**
   * load a zip file containing a run per subfolder
   *
   * @param zipPath
   * @return
   */
  def loadZip(zipPath: String, intensityThreshold: Double): Future[Int] = {

    // unzip the file
    val unzipPath = Unzip.unzip(new File(zipPath))

    // get the list of SearchIds to enter
    val resDirList = FileFinder.getListOfDirs(unzipPath)

    // insert one by one
    resDirList.foldLeft(Future{0})( (futureA, b) =>
      for {
        a <- futureA
        c <- insertRunFromPath(b, intensityThreshold)
      } yield {
        a + c
      })

  }

  /**
   * parse a subfolder containing *.mzIdentML, *.mzML, *.mgf
   *
   * @param runPath
   * @return
   */
  def insertRunFromPath(runPath: File, intensityThreshold: Double):Future[Int] = {

    // get the runId
    val runId: RunId = RunId("hoho")

    // check if all required files are here (it is not case sensitive)
    val requiredTypes = Set("mzid", "mzML", "mgf")
    val availableFiles = getRequiredFiles(requiredTypes, runPath)

    // now we insert all the data
    val ms1Iterator = LoaderMzML().parse(availableFiles.get("mzML").get, runId).filter(_.isLeft).map(_.left.get)
    val ms2Iterator = LoaderMGF.load(availableFiles.get("mgf").get, runId)
    val matchData = LoaderMzIdent.parse(availableFiles.get("mzid").get, SearchId(runId.value), runId)

    for{
      //first we insert MS data, because they're slow
      ms1Nr <- ExpMs1BinMongoDBService().insertMs1spectra(ms1Iterator, intensityThreshold)
      ms2Nr <- ExpMongoDBService().insertMs2spectra(ms2Iterator, runId)

      // and only last the other data
      matchNr <- MatchMongoDBService().insert(matchData._1)
      psmNumber <- ProteinMatchMongoDBService().insert(matchData._2)
      searchOk <- SearchInfoDBService().insert(matchData._3)

    }yield{
      ms1Nr + ms2Nr + matchNr + psmNumber + (if(searchOk) 1 else 0)
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
      val hit = if(l.size > 0) l(0) else throw new RuntimeException("Required file type is missing: " + x)
      (x, hit._2)
    }).toMap

  }


  /**
   * get the runId from the directory name
   *
   * @param runPath
   * @return
   */
  def getRunIdFromPath(runPath: File): String = {
    runPath.getAbsolutePath.split("\\/").last
  }


}




/**
 * the companion object
 */

object LoaderMascotData {

  def apply() = new LoaderMascotData

}