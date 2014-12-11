package ch.isbsib.proteomics.mzviz

/**
 * Mainly value class for experimental data
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
package object experimental {
  case class ScanNumber(value:Int) extends AnyVal
  case class IdRun(value:String) extends AnyVal
}
