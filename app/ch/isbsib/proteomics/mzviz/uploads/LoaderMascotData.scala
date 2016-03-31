package ch.isbsib.proteomics.mzviz.uploads

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.controllers.experimental.ExperimentalController._
import ch.isbsib.proteomics.mzviz.experimental.{MSRun, RunId}
import ch.isbsib.proteomics.mzviz.experimental.importer.{LoaderMGF, LoaderMzML, LoaderMzXML}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
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
  def loadZip(zipPath: String, intensityThreshold: Double): Future[Boolean] = {

    // unzip the file
    val unzipPath = Unzip.unzip(new File(zipPath))

    // get the list of SearchIds to enter
    val resDirList = FileFinder.getListOfDirs(unzipPath)

    // insert one by one
    resDirList.foldLeft(Future{true})( (futureA, b) =>
      for {
        a <- futureA
        c <- insertRunFromPath(b, 234)
      } yield {
        a & c
      })

  }

  /**
   * parse a subfolder containing *.mzIdentML, *.mzML, *.mgf
   *
   * @param runPath
   * @return
   */
  def insertRunFromPath(runPath: File, intensityThreshold: Double):Future[Boolean] = {

    // get the runId
    val runId: RunId = RunId("hoho")

    // check if all required files are here (it is not case sensitive)
    val requiredTypes = Set("mzid", "mzML", "mgf")
    val availableFiles = getRequiredFiles(requiredTypes, runPath)

    // now we insert all the data

    val ms1Iterator = LoaderMzML().parse(availableFiles.get("mzML").get, runId).filter(_.isLeft).map(_.left.get)
    val ms2Iterator = LoaderMGF.load(availableFiles.get("mgf").get, runId)

    for{
      //first we insert MS1 data, because they're slow
      resMs1Insertion <- ExpMs1BinMongoDBService().insertMs1spectra(ms1Iterator, intensityThreshold)

      // and only last the other data

    }yield{
      resMs1Insertion

    }

    //

    ???

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


}




/**
 * the companion object
 */

object LoaderMascotData {

  def apply() = new LoaderMascotData

}
