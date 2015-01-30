package ch.isbsib.proteomics.mzviz.modifications.importer

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics

 */
class UnimodParserTest extends Specification with ScalaFutures {


  "numberOfEntries" should {
    "return 1000"
      val n = UnimodParser("test/resources/unimod.xml").getSize
      n must equalTo(1000)

  }

}
