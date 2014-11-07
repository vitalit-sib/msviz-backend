package ch.isbsib.proteomics.mzviz.models

import org.specs2.mutable.Specification

/**
 * Created by admin on 07/11/14.
 */
class SpectrumIdSpecs extends Specification {
  "case" should {
    "need no new" in {
      val spid = SpectrumId(12, 123.45)
      spid.scanNumber must equalTo(12)
    }
  }
}
