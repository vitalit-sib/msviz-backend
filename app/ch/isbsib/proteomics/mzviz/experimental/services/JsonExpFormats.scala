package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.{ByteArrayInputStream, ObjectInputStream, ObjectOutputStream}

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.models._
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import org.apache.commons.codec.binary.Base64
import org.apache.commons.io.output.ByteArrayOutputStream
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonExpFormats {

  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._

  def encode64[T <: AnyVal](list: List[T]): String = {
    var baos = new ByteArrayOutputStream()
    var oos = new ObjectOutputStream(baos)
    oos.writeObject(list.toVector)
    oos.close()
    new String(Base64.encodeBase64(baos.toByteArray()))
  }

  def decode64[T <: AnyVal](str: String): List[T] = {
    val ois = new ObjectInputStream(
      new ByteArrayInputStream(Base64.decodeBase64(str)));
    ois.readObject().asInstanceOf[Vector[T]].toList
  }

  implicit val formatIntensityRank = new Format[IntensityRank] {
    override def reads(json: JsValue): JsResult[IntensityRank] = JsSuccess(IntensityRank(json.as[Int]))

    def writes(o: IntensityRank) = JsNumber(o.value)
  }


  implicit val formatMoz = new Format[Moz] {
    override def reads(json: JsValue): JsResult[Moz] = JsSuccess(Moz(json.as[Double]))

    def writes(o: Moz) = JsNumber(o.value)
  }

  implicit val formatIntensity = new Format[Intensity] {
    override def reads(json: JsValue): JsResult[Intensity] = JsSuccess(Intensity(json.as[Double]))

    def writes(o: Intensity) = JsNumber(o.value)
  }

  implicit val formatCharge = new Format[Charge] {
    override def reads(json: JsValue): JsResult[Charge] = JsSuccess(Charge(json.as[Int]))

    def writes(o: Charge) = JsNumber(o.value)
  }

  implicit val formatRetentionTime = new Format[RetentionTime] {
    override def reads(json: JsValue): JsResult[RetentionTime] = JsSuccess(RetentionTime(json.as[Double]))

    def writes(o: RetentionTime) = JsNumber(o.value)
  }

  implicit val formatMSLevel = new Format[MSLevel] {
    override def reads(json: JsValue): JsResult[MSLevel] = JsSuccess(MSLevel(json.as[Int]))

    def writes(o: MSLevel) = JsNumber(o.value)
  }

  implicit val formatScanNumber = new Format[ScanNumber] {
    override def reads(json: JsValue): JsResult[ScanNumber] = JsSuccess(ScanNumber(json.as[Int]))

    def writes(o: ScanNumber) = JsNumber(o.value)
  }

  implicit val formatRunId = new Format[RunId] {
    override def reads(json: JsValue): JsResult[RunId] = JsSuccess(RunId(json.as[String]))

    def writes(o: RunId) = JsString(o.value)
  }
  implicit val formatSpectrumUniqueId = new Format[SpectrumUniqueId] {
    override def reads(json: JsValue): JsResult[SpectrumUniqueId] = JsSuccess(SpectrumUniqueId(json.as[Int]))

    def writes(o: SpectrumUniqueId) = JsNumber(o.value)
  }


  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val formatExpPeakPrecursor = Json.format[ExpPeakPrecursor]
  implicit val formatExpPeakMSn = Json.format[ExpPeakMSn]
  implicit val formatSpectrumId = Json.format[SpectrumId]
  implicit val formatSpectrumRef = Json.format[SpectrumRef]
  implicit val formatMs1Entry = Json.format[Ms1Entry]

/*
  implicit val writesMs1Entry = new Writes[Ms1Entry] {

    def writes(o: Ms1Entry) = Json.obj(
      "runId" -> o.ref.value,
      "rt" -> o.rt.value,
      "intensity" -> o.intensity.value,
      "moz" -> o.moz.value
    )
  }
*/
  implicit val writesExpMSnSpectrum = new Writes[ExpMSnSpectrum] {

    def writes(o: ExpMSnSpectrum) = Json.obj(
      "ref" -> o.ref,
      "peaks" -> Json.obj(
        "msLevel" -> o.peaks.head.msLevel.value,
        "mozs" -> o.peaks.map(_.moz.value),
        "intensities" -> o.peaks.map(_.intensity.value),
        "intensityRanks" -> o.peaks.map(_.intensityRank.value)
      )
    )
  }

  //implicit val format = Json.format[]

}
