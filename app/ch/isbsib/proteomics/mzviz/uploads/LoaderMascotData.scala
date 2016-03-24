package ch.isbsib.proteomics.mzviz.uploads

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}

import scala.concurrent.Future
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File

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
