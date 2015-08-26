package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.commons._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
sealed trait ExpPeak {
  val moz: Moz
  val intensity: Intensity
  // val msLevel: MSLevel
}

// we want to have the the whole information for every peak
case class ExpPeakMS1(moz: Moz, intensity: Intensity) extends ExpPeak

case class ExpPeakMSn(moz: Moz, intensity: Intensity, intensityRank: IntensityRank, msLevel: MSLevel) extends ExpPeak

case class ExpPeakPrecursor(moz: Moz, intensity: Intensity, retentionTime: RetentionTime, charge: Charge) extends ExpPeak{
  val msLevel=MSLevel(1)
}
