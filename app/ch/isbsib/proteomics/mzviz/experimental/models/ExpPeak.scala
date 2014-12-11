package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.commons._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
sealed trait ExpPeak {
  val moz: Moz
  val intensity: Intensity
  val msLevel: MSLevel
}

case class ExpPeakMSn(moz: Moz, intensity: Intensity, intensityRank: IntensityRank, msLevel: MSLevel) extends ExpPeak

case class ExpPeakPrecursor(moz: Moz, intensity: Intensity, retentionTime: RetentionTime, charge: Charge) extends ExpPeak{
  val msLevel=MSLevel(1)
}
