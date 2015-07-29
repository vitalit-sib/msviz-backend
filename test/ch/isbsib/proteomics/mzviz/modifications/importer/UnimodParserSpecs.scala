package ch.isbsib.proteomics.mzviz.modifications.importer

import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics

 */
class UnimodParserSpecs extends Specification with ScalaFutures {


  "unimod" should {
    val filename=("test/resources/modifications/unimod.xml")
    val n = UnimodParser(filename).getDictionarySize
    val value=Some(42.010565)

    "numberOfEntries should be 1000" in {
      n must equalTo(1000)
    }


    "Acetylation should return 42.010565" in {
     UnimodParser(filename).getValue("Acetylation") must equalTo(value)
    }
  }

}
