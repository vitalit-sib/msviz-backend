package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.theoretical.{ProteinIdentifier, SequenceSource, AccessionCode}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.json.Json
import ch.isbsib.proteomics.mzviz.modifications.services.JsonModificationFormats._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

object JsonMatchFormats {


  implicit val formatSpectraId = new Format[SpectrumId] {
    override def reads(json: JsValue): JsResult[SpectrumId] = JsSuccess(SpectrumId(SpectrumUniqueId((json \ "id").as[String]), RunId((json \ "runId").as[String])))

    def writes(o: SpectrumId) = Json.obj("id" -> o.id.value, "runId" -> o.runId.value)
  }

  implicit val formatAccessionCode = new Format[AccessionCode] {
    override def reads(json: JsValue): JsResult[AccessionCode] = JsSuccess(AccessionCode(json.as[String]))

    def writes(o: AccessionCode) = JsString(o.value)
  }
  implicit val formatProteinIdentifier = new Format[ProteinIdentifier] {
    override def reads(json: JsValue): JsResult[ProteinIdentifier] = JsSuccess(ProteinIdentifier(json.as[String]))

    def writes(o: ProteinIdentifier) = JsString(o.value)
  }

  implicit val formatSequenceSource = new Format[SequenceSource] {
    override def reads(json: JsValue): JsResult[SequenceSource] = JsSuccess(SequenceSource(json.as[String]))

    def writes(o: SequenceSource) = JsString(o.value)
  }

  implicit val formatSearchId = new Format[SearchId] {
    override def reads(json: JsValue): JsResult[SearchId] = JsSuccess(SearchId(json.as[String]))

    def writes(o: SearchId) = JsString(o.value)
  }

  implicit val formatProteinRef = new Format[ProteinRef] {
    //OK, that starts to be ugly. identifiers field might not have been defined for everyone...
    override def reads(json: JsValue): JsResult[ProteinRef] = {
      JsSuccess(ProteinRef(
        AC = AccessionCode((json \ "AC").as[String]),
        identifiers = (json \ "identifiers").asOpt[Set[String]].getOrElse(Set()).map(ProteinIdentifier.apply),
        source = (json \ "source").asOpt[String].map(SequenceSource.apply)
      ))
    }

    def writes(o: ProteinRef) = Json.obj(
      "AC" -> o.AC,
      "identifiers" -> o.identifiers,
      "source" -> o.source
    )

  }


  implicit val formatProteinMatch = Json.format[ProteinMatch]
  implicit val formatPeptide = Json.format[Peptide]
  implicit val formatPepMatchInfo = Json.format[PepMatchInfo]
  implicit val formatPepSpectraMatch = Json.format[PepSpectraMatch]
  implicit val formatSearchInfo = Json.format[SearchInfo]

}
