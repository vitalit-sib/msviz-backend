package ch.isbsib.proteomics.mzviz.qc.services
import java.util
import ch.isbsib.proteomics.mzviz.qc._
import ch.isbsib.proteomics.mzviz.qc.models.{Quantity, QcSummaryEntry, RawfileInfomation}
import play.api.libs.json._

/**
 * Created by Qinfang Jolliet on 05/12/14.
 * @author Roman Mylonas, Trinidad Martin  & Qinfang Jolliet & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonQCFormats {

  import play.api.libs.json.Json
  implicit val formatProteinName = new Format[ProteinName] {
    override def reads(json: JsValue): JsResult[ProteinName] = JsSuccess(ProteinName(json.as[String]))

    def writes(o: ProteinName) = JsString(o.value)
  }
  implicit val formatProteinQuantity= new Format[ProteinQuantity] {
    override def reads(json: JsValue): JsResult[ProteinQuantity] = JsSuccess(ProteinQuantity(json.as[String]))

    def writes(o: ProteinQuantity) = JsString(o.value)
  }
  implicit val formatMachineName = new Format[MachineName] {
    override def reads(json: JsValue): JsResult[MachineName] = JsSuccess(MachineName(json.as[String]))

    def writes(o: MachineName) = JsString(o.value)
  }
  implicit val formatColumnType = new Format[ColumnType] {
    override def reads(json: JsValue): JsResult[ColumnType] = JsSuccess(ColumnType(json.as[String]))

    def writes(o: ColumnType) = JsString(o.value)
  }

  implicit val formatQcDate = new Format[QcDate] {
    override def reads(json: JsValue): JsResult[QcDate] = JsSuccess(QcDate(json.as[util.Date]))

    def writes(o: QcDate) = JsString(o.toString)
  }
  implicit val formatQcIndex = new Format[QcIndex] {
    override def reads(json: JsValue): JsResult[QcIndex] = JsSuccess(QcIndex(json.as[String]))

    def writes(o: QcIndex) = JsString(o.value)
  }
  implicit val formatQuantity =  Json.format[Quantity]
  implicit val formatRawFileInformation =  Json.format[RawfileInfomation]
  implicit val formatSummary =  Json.format[QcSummaryEntry]

}
