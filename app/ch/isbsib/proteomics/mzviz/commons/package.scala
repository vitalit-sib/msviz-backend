package ch.isbsib.proteomics.mzviz

/**
 * @author Alexandre Masselot
 */
package object commons {
  case class Moz(value:Double) extends AnyVal

  case class Intensity(value:Double) extends AnyVal

  case class IntensityRank(value:Int) extends AnyVal

  case class Charge(value:Int) extends AnyVal

  case class MSLevel(value:Int) extends AnyVal

  case class RetentionTime(value:Double) extends AnyVal
}
