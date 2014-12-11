package ch.isbsib.proteomics.mzviz.controllers

import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.libs.json.{JsValue, Writes}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
object JsonCommonsFormats {

  import play.api.libs.json.Json

  implicit val jsonWritesThrowable = new Writes[Throwable] {
    override def writes(o: Throwable): JsValue = {
      Json.obj("message" -> o.getMessage, "stackTrace" -> o.getStackTrace.toList.map(_.toString))
    }
  }

  implicit val jsonWritesMap = new Writes[Map[String, Int]] {
    override def writes(o: Map[String, Int]): JsValue = {
      o.foldLeft(Json.obj())({ (acc, p) => acc ++ Json.obj(p._1 -> p._2)})
    }
  }

  //  implicit val formatAccessionCode = new Format[AccessionCode] {
//    override def reads(json: JsValue): JsResult[AccessionCode] = JsSuccess(AccessionCode(json.as[String]))
//
//    def writes(o: AccessionCode) = JsString(o.value)
//  }
implicit val formatAC = Json.format[AccessionCode]
  implicit val formatSequenceSource = Json.format[SequenceSource]
  implicit val formatFastaEntry = Json.format[FastaEntry]

}
