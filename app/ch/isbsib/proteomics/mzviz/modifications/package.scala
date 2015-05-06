package ch.isbsib.proteomics.mzviz

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
package object modifications {

  case class ModifName(value:String) extends AnyVal{
    override def toString=value
  }

}
