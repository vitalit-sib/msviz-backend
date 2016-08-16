package ch.isbsib.proteomics.mzviz.commons

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.{EvidenceTableEntry, ProteinGroupsTableEntry}
import ch.isbsib.proteomics.mzviz.matches.models.{PepSpectraMatch, ProteinIdent}
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas & Trinidad Martin
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class UnzipSpecs extends Specification {

  "unzip file" in {

    val zipFile = new File("test/resources/commons/tiny.zip")

    val unzippedDir = Unzip.unzip(zipFile)

    val files = FileFinder.getListOfDirs(unzippedDir)

    files.length mustEqual(1)
    files(0).getName mustEqual("txt")

  }

}
