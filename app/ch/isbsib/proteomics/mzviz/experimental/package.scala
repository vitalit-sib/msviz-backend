package ch.isbsib.proteomics.mzviz

/**
 * Mainly value class for experimental data
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
package object experimental {
  case class ScanNumber(value:Int) extends AnyVal
  case class RunId(value:String) extends AnyVal

  /**
   * shuld be unique across a run (scan number or title)s
   * @param value
   */
  case class SpectrumUniqueId(value:String) extends AnyVal
}
