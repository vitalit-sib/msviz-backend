package ch.isbsib.proteomics.mzviz

/**
 * Mainly value class for experimental data
 * @author Alexandre Masselot
 */
package object experimental {
  case class ScanNumber(value:Int) extends AnyVal
  case class IdRun(value:String) extends AnyVal
}
