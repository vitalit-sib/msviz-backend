package ch.isbsib.proteomics.mzviz.uploads

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent

import scala.concurrent.Future
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global

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
  def loadZip(zipPath: String): Future[Boolean] = {

    // unzip the file
    val unzipPath = Unzip.unzip(new File(zipPath))

    // get the list of SearchIds to enter
    val resDirList = FileFinder.getListOfDirs(unzipPath)

    // insert one by one
    resDirList.foldLeft(Future{true})( (futureA, b) =>
      for {
        a <- futureA
        c <- insertRunFromPath(b)
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
  def insertRunFromPath(runPath: File):Future[Boolean] = {

    // check if all required files are here (it is not case sensitive)
    val requiredTypes = Set("mzid", "mzML", "mgf")
    val availableFiles = getRequiredFiles(requiredTypes, runPath)

    if(availableFiles.size != requiredTypes.size){
      throw new RuntimeException("There are some required files missing in [" + runPath.getName + "]. Following file types are needed: " + requiredTypes.toString)
    }

    // first we insert matches (they're fast)
    //LoaderMzIdent.parse(request.body.file, SearchId(searchId), rid)

    ???

  }


  /**
   * get the required files as a hashmap
   *
   * @param fileTypes
   * @param runPath
   */
  def getRequiredFiles(fileTypes: Set[String], runPath: File): Map[String, File] = {

    val availableFiles = FileFinder.getListOfFiles(runPath.getAbsolutePath)

    // get file extensions as lower case
    val extensions =  availableFiles.map(x =>  x.toString.substring(x.toString.lastIndexOf(".") + 1).toLowerCase)

    // find the pair for every file type
    val listFound = fileTypes.toList.map(x => extensions.filter(_ == x.toLowerCase)).zipWithIndex

    //val listFound = fileTypes.toList.map(x => extensions.contains(x.toLowerCase))

    // check if all are true
    listFound.forall(x => x)

  }


}




/**
 * the companion object
 */

object LoaderMascotData {

  def apply() = new LoaderMascotData

}
