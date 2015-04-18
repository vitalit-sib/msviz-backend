package ch.isbsib.proteomics.mzviz.spectrasim.services

import ch.isbsib.proteomics.mzviz.experimental.models.{ExpPeakPrecursor, SpectrumRef, ExpMSnSpectrum}
import ch.isbsib.proteomics.mzviz.spectrasim.models.{SpSpRefMatch, SpSpMatch}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import play.api.libs.json._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonSimFormats {

//  implicit val formatExpPeakPrecursor = Json.format[ExpPeakPrecursor]
//
//  implicit val formatSpectrumRef = Json.format[SpectrumRef]
//
//  implicit val formatExpMSnSpectrum = Json.format[ExpMSnSpectrum]

  implicit val formatSpSpRefMatch = Json.format[SpSpRefMatch]

  implicit val writesSpSpMatch = Json.writes[SpSpMatch]

}
