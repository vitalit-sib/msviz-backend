package ch.isbsib.proteomics.mzviz

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
package object matches {
  case class SearchId(value:String) extends AnyVal

  case class HitRank(value:Int) extends AnyVal
}
