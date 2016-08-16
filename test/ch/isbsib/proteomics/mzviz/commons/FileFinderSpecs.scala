package ch.isbsib.proteomics.mzviz.commons

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas & Trinidad Martin
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class FileFinderSpecs extends Specification {

  "list dirs" in {

    val dir = "test/resources/uploads"

    val files = FileFinder.getListOfDirs(dir)

    files.length mustEqual(2)

  }

  "list files" in {

    val dir = "test/resources/uploads"

    val files = FileFinder.getListOfFiles(dir)

    files.length mustEqual(4)

  }

  "highest dir" in {

    val dir = "test/resources/uploads"
    val highest = FileFinder.getHighestDir(dir)

    highest.split("\\/").last mustEqual("mascot_test")

  }



}
