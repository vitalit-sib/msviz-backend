package ch.isbsib.proteomics.mzviz

/**
 * Mainly value class for experimental data
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
package object experimental {
  case class ScanNumber(value:Int) extends AnyVal
  case class RunId(value:String) extends AnyVal

  /**
   * should be unique across a run (scan number or title)s
   * @param value just a value class
   */
  case class SpectrumUniqueId(value:String) extends AnyVal
}
