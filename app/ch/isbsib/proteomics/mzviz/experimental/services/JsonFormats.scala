package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.models._
import play.api.libs.json._

/**
 * @author Alexandre Masselot
 */
object JsonFormats {

  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._

  implicit val formatIntensityRank = new Format[IntensityRank] {
    override def reads(json: JsValue): JsResult[IntensityRank] = ???

    def writes(o: IntensityRank) = JsNumber(o.value)
  }


  implicit val formatMoz = new Format[Moz] {
    override def reads(json: JsValue): JsResult[Moz] = ???

    def writes(o: Moz) = JsNumber(o.value)
  }

  implicit val formatIntensity = new Format[Intensity] {
    override def reads(json: JsValue): JsResult[Intensity] = ???

    def writes(o: Intensity) = JsNumber(o.value)
  }

  implicit val formatCharge = new Format[Charge] {
    override def reads(json: JsValue): JsResult[Charge] = ???

    def writes(o: Charge) = JsNumber(o.value)
  }

  implicit val formatRetentionTime = new Format[RetentionTime] {
    override def reads(json: JsValue): JsResult[RetentionTime] = ???

    def writes(o: RetentionTime) = JsNumber(o.value)
  }

  implicit val formatMSLevel = new Format[MSLevel] {
    override def reads(json: JsValue): JsResult[MSLevel] = ???

    def writes(o: MSLevel) = JsNumber(o.value)
  }

  implicit val formatScanNumber = new Format[ScanNumber] {
    override def reads(json: JsValue): JsResult[ScanNumber] = ???

    def writes(o: ScanNumber) = JsNumber(o.value)
  }

  implicit val formatIdRun = new Format[IdRun] {
    override def reads(json: JsValue): JsResult[IdRun] = ???

    def writes(o: IdRun) = JsString(o.value)
  }

  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val formatExpPeakPrecursor = Json.format[ExpPeakPrecursor]
  implicit val formatExpPeakMSn = Json.format[ExpPeakMSn]
  implicit val formatRefSpectrum = Json.format[RefSpectrum]

  implicit val formatExpMSnSpectrum = new Format[ExpMSnSpectrum] {
    override def reads(json: JsValue): JsResult[ExpMSnSpectrum] = ???

    def writes(o: ExpMSnSpectrum) = Json.obj(
      "ref" -> o.ref,
      "peaks" -> Json.obj(
        "msLevel" -> o.peaks.head.msLevel,
        "mozs" -> o.peaks.map(_.moz),
        "intensities" -> o.peaks.map(_.intensity),
        "intensityRanks" -> o.peaks.map(_.intensityRank)
      )
    )
  }

  //implicit val format = Json.format[]

}
