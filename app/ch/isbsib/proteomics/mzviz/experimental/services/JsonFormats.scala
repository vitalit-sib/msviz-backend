package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.models._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}

/**
 * @author Alexandre Masselot
 */
object JsonFormats {

  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._


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
    override def reads(json: JsValue): JsResult[MSLevel] =JsSuccess(MSLevel(json.as[Int]))

    def writes(o: MSLevel) = JsNumber(o.value)
  }

  implicit val formatScanNumber = new Format[ScanNumber] {
    override def reads(json: JsValue): JsResult[ScanNumber] = JsSuccess(ScanNumber(json.as[Int]))

    def writes(o: ScanNumber) = JsNumber(o.value)
  }

  implicit val formatIdRun = new Format[IdRun] {
    override def reads(json: JsValue): JsResult[IdRun] = JsSuccess(IdRun(json.as[String]))

    def writes(o: IdRun) = JsString(o.value)
  }

  implicit val writesMap = new Writes[Map[String, Int]] {
    override def writes(o: Map[String, Int]): JsValue = {
      o.foldLeft(Json.obj())({ (acc, p) => acc ++ Json.obj(p._1 -> p._2)})
    }
  }


  // Generates Writes and Reads for Feed and User thanks to Json Macros
  implicit val formatExpPeakPrecursor = Json.format[ExpPeakPrecursor]
  implicit val formatExpPeakMSn = Json.format[ExpPeakMSn]
  implicit val formatRefSpectrum = Json.format[RefSpectrum]


  implicit val formatExpMSnSpectrum = new Format[ExpMSnSpectrum] {
    override def reads(json: JsValue): JsResult[ExpMSnSpectrum] = {
      val ref = (JsPath \ "ref").read[RefSpectrum].reads(json).get
      val msLevel = MSLevel(json.validate[Int]((JsPath \ "peaks" \ "msLevel").read[Int]).get)

      //re-assemble the peaks
      val dtlistReads =
        (JsPath \ "peaks" \ "mozs").read[List[Double]] and
          (JsPath \ "peaks" \ "intensities").read[List[Double]] and
          (JsPath \ "peaks" \ "intensityRanks").read[List[Int]] tupled

      val dt = dtlistReads.reads(json).get
      val peaks:List[ExpPeakMSn] =
        for {
          ((m:Double, i:Double), r:Int) <- dt._1.zip(dt._2).zip(dt._3)
        } yield {
          ExpPeakMSn(moz = Moz(m), intensity = Intensity(i), intensityRank = IntensityRank(r), msLevel = msLevel)
        }

      JsSuccess(
        ExpMSnSpectrum(ref = ref, peaks = peaks)
      )

    }

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
