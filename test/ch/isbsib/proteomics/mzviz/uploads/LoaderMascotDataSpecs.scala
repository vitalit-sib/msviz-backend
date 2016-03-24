package ch.isbsib.proteomics.mzviz.uploads


import org.specs2.mutable.Specification
import java.io.{IOException, File}



/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMascotDataSpecs extends Specification {

  "check required files" in {

    val dir = new File ("test/resources/uploads/sample1")

    val requiredFilesMap = LoaderMascotData().getRequiredFiles(Set("mzid", "mgf", "mzml"), dir)

    requiredFilesMap.get("mzid").get.getName mustEqual ("sample1.mzid")
    requiredFilesMap.get("mgf").get.getName mustEqual ("sample1.mgf")
    requiredFilesMap.get("mzml").get.getName mustEqual ("sample1.mzML")

  }


}
