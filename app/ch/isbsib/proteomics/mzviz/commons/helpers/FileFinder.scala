package ch.isbsib.proteomics.mzviz.commons.helpers

import java.io.File

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object FileFinder {

  /**
   * give back the list of directories found at the given path
   * @param dir
   * @return list of directories
   */
  def getListOfDirs(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isDirectory).toList
    } else {
      List[File]()
    }
  }

  /**
   * give back the list of files found at the given path
   * @param dir
   * @return list of files
   */
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  /**
   * recursively look for the right directory to start from
   * @param baseDir
   * @return
   */
  def getHighestDir(baseDir: String):String = {
    val innerDirs = FileFinder.getListOfDirs(baseDir)
    if(innerDirs.size == 0) return baseDir
    // we're just looking at the first directory
    else getHighestDir(innerDirs(0).getAbsolutePath)
  }

}
