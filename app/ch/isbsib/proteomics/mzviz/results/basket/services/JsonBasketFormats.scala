package ch.isbsib.proteomics.mzviz.results.basket.services

import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.results.basket.models.{XicPeak, RtRange, BasketEntry}
import play.api.libs.json._
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonBasketFormats {


  implicit val formatRtRange = Json.format[RtRange]
  implicit val formatXicPeak = Json.format[XicPeak]

  import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats.formatAccessionCode

  implicit val formatBasketEntry = Json.format[BasketEntry]


}
