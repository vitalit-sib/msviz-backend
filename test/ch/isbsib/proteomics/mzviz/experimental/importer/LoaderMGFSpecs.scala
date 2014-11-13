package ch.isbsib.proteomics.mzviz.experimental.importer

import org.specs2.mutable.Specification
import ch.isbsib.proteomics.mzviz.experimental._

/**
 * @author Alexandre Masselot
 */
class LoaderMGFSpecs extends Specification {
  "load" should {
    val run = LoaderMGF.load("test/resources/F001644.mgf")
    "get idRun out of filename" in {
      run.id must equalTo(IdRun("F001644"))
    }
    "count the msms" in {
      run.msnSpectra.size must equalTo(1822)
    }
  }
}
