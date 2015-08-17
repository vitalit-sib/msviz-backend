package ch.isbsib.proteomics.mzviz.qc.models

import org.specs2.mutable.Specification

/**
 * Created by qjolliet on 06/08/15.
 */
class QuantitySpecs extends Specification{

  "Qunatity" should{
    "create " in {
      val q = Quantity(300, "ng")
      q.value must beEqualTo(300)
      q.unit must beEqualTo("ng")
    }

    "parse '300ng'" in {
      val tq =Quantity.parse("300ng")
      tq must beASuccessfulTry
      val q = tq.get
      q.value must beEqualTo(300)
      q.unit must beEqualTo("ng")
    }

    "parse 'paf' should fail" in {
      val tq =Quantity.parse("paf")
      tq must beAFailedTry
    }
  }
}
