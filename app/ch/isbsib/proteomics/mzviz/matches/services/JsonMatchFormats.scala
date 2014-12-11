package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.{SpectraSource, SpectraId}
import ch.isbsib.proteomics.mzviz.matches.ProteinAC
import ch.isbsib.proteomics.mzviz.matches.models.{Peptide, PepMatchInfo, ProteinMatch, PepSpectraMatch}
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

  implicit val formatProteinAC = new Format[ProteinAC] {
    override def reads(json: JsValue): JsResult[ProteinAC] = JsSuccess(ProteinAC(json.as[String]))

    def writes(o: ProteinAC) = JsString(o.value)
  }

  implicit val formatProteinMatch = Json.format[ProteinMatch]
  implicit val formatPeptide = Json.format[Peptide]
  implicit val formatPepMatchInfo = Json.format[PepMatchInfo]
  implicit val formatPepSpectraMatch = Json.format[PepSpectraMatch]


}
