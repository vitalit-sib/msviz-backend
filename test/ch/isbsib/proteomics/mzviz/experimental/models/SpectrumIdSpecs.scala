package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.commons._
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2017, SIB Swiss Institute of Bioinformatics
 */
class SpectrumIdSpecs extends Specification {
  "case" should {
    "need no new" in {
      val spid = SpectrumRef(Some(ScanNumber(12)),
        ExpPeakPrecursor(moz = Moz(123.45), intensity=Intensity(678), retentionTime=RetentionTime(123.45), charge=Charge(2), None, molecularMass=Some(MolecularMass(244.88544)), None), "no title", SpectrumId(SpectrumUniqueId("12"), RunId("Paf")))
      spid.scanNumber must equalTo(Some(ScanNumber(12)))
    }
  }
}
