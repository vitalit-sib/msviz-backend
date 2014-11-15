package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.models._

/**
 * @author Alexandre Masselot
 */
object JsonFormats {
  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._


  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val formatIntensityRank = Json.format[IntensityRank]
  implicit val formatMoz = Json.format[Moz]
  implicit val formatIntensity = Json.format[Intensity]
  implicit val formatCharge = Json.format[Charge]
  implicit val formatRetentionTime = Json.format[RetentionTime]
  implicit val formatMSLevel = Json.format[MSLevel]
  implicit val formatScanNumber = Json.format[ScanNumber]
  implicit val formatExpPeakPrecursor = Json.format[ExpPeakPrecursor]
  implicit val formatExpPeakMSn = Json.format[ExpPeakMSn]
  implicit val formatRefSpectrum = Json.format[RefSpectrum]
  implicit val formatExpMSnSpectrum = Json.format[ExpMSnSpectrum]
  //implicit val format = Json.format[]

}
