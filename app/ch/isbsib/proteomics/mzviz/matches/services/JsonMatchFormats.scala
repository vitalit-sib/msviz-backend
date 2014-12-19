package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.{SpectraSource, SpectraId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import play.api.libs.json._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */

object JsonMatchFormats {

  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._


  implicit val formatSpectraId = new Format[SpectraId] {
    override def reads(json: JsValue): JsResult[SpectraId] = JsSuccess(SpectraId(json.as[String]))

    def writes(o: SpectraId) = JsString(o.value)
  }

  implicit val formatSpectraSource = new Format[SpectraSource] {
    override def reads(json: JsValue): JsResult[SpectraSource] = JsSuccess(SpectraSource(json.as[String]))

    def writes(o: SpectraSource) = JsString(o.value)
  }

  implicit val formatAccessionCode = new Format[AccessionCode] {
    override def reads(json: JsValue): JsResult[AccessionCode] = JsSuccess(AccessionCode(json.as[String]))

    def writes(o: AccessionCode) = JsString(o.value)
  }

  implicit val formatSequenceSource = new Format[SequenceSource] {
    override def reads(json: JsValue): JsResult[SequenceSource] = JsSuccess(SequenceSource(json.as[String]))

    def writes(o: SequenceSource) = JsString(o.value)
  }

  implicit val formatSearchId = new Format[SearchId] {
    override def reads(json: JsValue): JsResult[SearchId] = JsSuccess(SearchId(json.as[String]))

    def writes(o: SearchId) = JsString(o.value)
  }

  implicit val formatProteinRef = Json.format[ProteinRef]
  implicit val formatProteinMatch = Json.format[ProteinMatch]
  implicit val formatPeptide = Json.format[Peptide]
  implicit val formatPepMatchInfo = Json.format[PepMatchInfo]
  implicit val formatPepSpectraMatch = Json.format[PepSpectraMatch]


}
