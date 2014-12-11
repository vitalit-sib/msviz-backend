package ch.isbsib.proteomics.mzviz.theoretical.services

import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.libs.json._

/**
 * Created by tmartinc on 05/12/14.
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
object JsonTheoFormats {

  import play.api.libs.json.Json

  implicit val formatAccessionCode = new Format[AccessionCode] {
    override def reads(json: JsValue): JsResult[AccessionCode] = JsSuccess(AccessionCode(json.as[String]))

    def writes(o: AccessionCode) = JsString(o.value)
  }

  implicit val formatSequenceSource = new Format[SequenceSource] {
    override def reads(json: JsValue): JsResult[SequenceSource] = JsSuccess(SequenceSource(json.as[String]))

    def writes(o: SequenceSource) = JsString(o.value)
  }

  implicit val jsonSequenceSource = new Writes[SequenceSource] {
    override def writes(o: SequenceSource): JsValue = {
      JsString(o.value)
    }
  }


  implicit val formatFastaEntry = Json.format[FastaEntry]

}
