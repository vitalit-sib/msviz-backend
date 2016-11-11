package ch.isbsib.proteomics.mzviz.commons

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
sealed trait MassUnit {def value: String}
case object PPM extends MassUnit {val value = "ppm"}
case object Dalton extends MassUnit {val value = "dalton"}

//case class MassUnit(value:String) extends AnyVal