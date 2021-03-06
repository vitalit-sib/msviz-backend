package ch.isbsib.proteomics.mzviz.commons.helpers

import java.io.{IOException, File}
import net.lingala.zip4j.core.ZipFile

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object Unzip {

  /**
   * create a temporary directory and return the absolute path as a String
   *
   * @return absolute path
   */
  def createTmpDir:String = {
    val tmpDir = File.createTempFile("temp", "zip")

    if(!(tmpDir.delete()))
    {
      throw new IOException("Could not delete temp file: " + tmpDir.getAbsolutePath());
    }

    if(!(tmpDir.mkdir()))
    {
      throw new IOException("Could not create temp directory: " + tmpDir.getAbsolutePath());
    }

    return tmpDir.getAbsolutePath
  }

  /**
   * extract the given ZIP file and give back the absolute path to which the content was extracted
   *
   * @param zipFile
   * @return path to extraction directory
   */
  def unzip(zipFile: File): String = {

    val zip:ZipFile  = new ZipFile(zipFile)
    val tmpDir = createTmpDir

    zip.extractAll(tmpDir)

    tmpDir
  }

}
