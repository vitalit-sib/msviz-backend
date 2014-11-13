package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.commons._
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas
 */
class SpectrumIdSpecs extends Specification {
  "case" should {
    "need no new" in {
      val spid = RefSpectrum(ScanNumber(12), RetentionTime(123.45))
      spid.scanNumber must equalTo(12)
    }
  }
}
