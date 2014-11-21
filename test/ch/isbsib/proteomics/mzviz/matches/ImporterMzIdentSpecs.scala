package ch.isbsib.proteomics.mzviz.matches.importer

import org.specs2.mutable.Specification

/**
 *  @author Alexandre Masselot
 */

class ImporterMzIdentSpecs extends Specification {
    "read" should {
      "get something" in {
        ImporterMzIdent.parse("test/resources/M_100.mzid") must equalTo(99999)
      }
    }
}
