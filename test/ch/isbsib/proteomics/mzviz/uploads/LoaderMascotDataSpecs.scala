package ch.isbsib.proteomics.mzviz.uploads


import org.specs2.mutable.Specification
import java.io.{IOException, File}



/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMascotDataSpecsextends extends Specification {

  "check required files" in {

    val dir = new File ("test/resources/uploads/sample1")

    val isOk = LoaderMascotData().checkRequiredFiles(Set("mzid", "mzML", "mgf"), dir)

    isOk mustEqual(true)

  }


}
