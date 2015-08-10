package ch.isbsib.proteomics.mzviz.qc.models

import scala.util.{Failure, Success, Try}

/**
 *
 * This handles quantity, with value and unit
 * Created by qjolliet on 06/08/15.
 */
case class Quantity(value: Double, unit: String)

case class CannotParseQuantityException(str: String) extends Exception(str)

object Quantity {
  val re = """([\d\.]+)([a-zA-Z]+)""".r

  def parse(str: String): Try[Quantity] = str match {
    case re(value, unit) => Success(Quantity(value.toDouble, unit))
    case x => Failure(CannotParseQuantityException(x))
  }
}
