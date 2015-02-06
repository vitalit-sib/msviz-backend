package ch.isbsib.proteomics.mzviz.modifications.importer

import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics

 */
class UnimodParserSpecs extends Specification with ScalaFutures {


  "unimod" should {
    val filename="http://mascot.vital-it.ch/mascot/cgi/get_params.pl?Show=MS_UNIMODXML"
    val n = UnimodParser(filename).getSize
    val value=List("42.01565", "42.036")

    "numberOfEntries" in {
      n must equalTo(1000)
    }

    "Acetylation should return 42.010565, 42.036" in {
       UnimodParser(filename).getValues("Acetylation") must equalTo(value)
    }



  }

}
