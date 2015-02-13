package ch.isbsib.proteomics.mzviz.modifications.importer

import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics

 */
class UnimodParserSpecs extends Specification with ScalaFutures {


  "unimod" should {
    val filename=("test/resources/unimod.xml")
    val n = UnimodParser(filename).getSize
    val value=Some(List(Tuple2("42.010565", "42.0367")))

    "numberOfEntries" in {
      n must equalTo(1000)
    }

    "Acetylation should return 42.010565, 42.036" in {
     UnimodParser(filename).getValues("Acetylation") must equalTo(value)
    }



  }

}
