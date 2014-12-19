package ch.isbsib.proteomics.mzviz

/**
 * value classes to be fancy
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
package object commons {
  case class Moz(value:Double) extends AnyVal

  case class Intensity(value:Double) extends AnyVal

  case class IntensityRank(value:Int) extends AnyVal

  case class Charge(value:Int) extends AnyVal

  case class MSLevel(value:Int) extends AnyVal

  case class RetentionTime(value:Double) extends AnyVal

  case class SpectraId(value:String) extends AnyVal

  case class SpectraSource(value:String) extends AnyVal

}
