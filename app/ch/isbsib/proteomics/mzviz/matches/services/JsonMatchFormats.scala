package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.{Dalton, MassUnit, PPM}
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.{HitRank, SearchId}
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, ProteinIdentifier, SequenceSource}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.json.Json
import ch.isbsib.proteomics.mzviz.modifications.services.JsonModificationFormats._
import play.api.libs.json.Json.JsValueWrapper

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

object JsonMatchFormats {


  implicit val formatSpectraId = new Format[SpectrumId] {
    override def reads(json: JsValue): JsResult[SpectrumId] = JsSuccess(SpectrumId(SpectrumUniqueId((json \ "id").as[String]), RunId((json \ "runId").as[String])))

    def writes(o: SpectrumId) = Json.obj("id" -> o.id.value, "runId" -> o.runId.value)
  }

  implicit val formatHitRank = new Format[HitRank] {
    override def reads(json: JsValue): JsResult[HitRank] = JsSuccess(HitRank(json.as[Int]))

    def writes(o: HitRank) = JsNumber(o.value)
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

  implicit val formatMassUnit = new Format[MassUnit] {
    override def reads(json: JsValue): JsResult[MassUnit] = {
      JsSuccess(json.as[String] match{
        case "ppm" => PPM
        case "dalton" => Dalton
      })
    }

    def writes(o: MassUnit) = JsString(o.value)
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


  implicit val formatModificationPobabilities = new Format[Map[ModifName, String]] {
    override def reads(json: JsValue): JsResult[Map[ModifName, String]] = {
      JsSuccess(json.as[Map[String, String]].map({ case (k, v) =>
          ModifName(k) -> v
      }))
    }

    def writes(o: Map[ModifName, String]) = Json.obj(
      o.map({ case(s, o) =>
          val ret: (String, JsValueWrapper) = s.value -> JsString(o)
          ret
      }).toSeq:_*
    )

  }

  implicit val formatHighestModifProbability = new Format[Map[ModifName, Double]] {
    override def reads(json: JsValue): JsResult[Map[ModifName, Double]] = {
      JsSuccess(json.as[Map[String, Double]].map({ case (k, v) =>
        ModifName(k) -> v
      }))
    }

    def writes(o: Map[ModifName, Double]) = Json.obj(
      o.map({ case(s, o) =>
        val ret: (String, JsValueWrapper) = s.value -> JsNumber(o)
        ret
      }).toSeq:_*
    )

  }

  implicit val formatModifStatus = new Format[ModifStatus] {
    override def reads(json: JsValue): JsResult[ModifStatus] = {
      JsSuccess(json.as[String] match{
        case "MAIN" => MAIN
        case "CONFLICT" => CONFLICT
      })
    }

    def writes(o: ModifStatus) = JsString(o.value)
  }

  implicit val formatModifInfo = Json.format[ModifInfo]

  implicit val formatModificationProbabilityInfo = new Format[Map[ModifName, Seq[ModifInfo]]] {
    override def reads(json: JsValue): JsResult[Map[ModifName, Seq[ModifInfo]]] = {
      JsSuccess(json.as[Map[String, Seq[ModifInfo]]].map({ case (k, v) =>
        ModifName(k) -> v
      }))
    }

    def writes(o: Map[ModifName, Seq[ModifInfo]]) = Json.obj(
      o.map({ case(s:ModifName, o:Seq[ModifInfo]) =>
        val ret: (String, JsValueWrapper) = s.value -> JsArray(o.map({case mi:ModifInfo => Json.toJson(mi)}))
        ret
      }).toSeq:_*
    )

  }

  implicit val formatProteinMatch = Json.format[ProteinMatch]
  implicit val formatPeptide = Json.format[Peptide]
  implicit val formatIdentScore = Json.format[IdentScore]
  implicit val formatPepMatchInfo = Json.format[PepMatchInfo]
  implicit val formatPepSpectraMatch = Json.format[PepSpectraMatch]
  implicit val formatSearchDatabase = Json.format[SearchDatabase]
  implicit val formatSubmissionStatus = Json.format[SubmissionStatus]
  implicit val formatSearchInfo = Json.format[SearchInfo]
  implicit val formatProteinIdentInfo = Json.format[ProteinIdentInfo]
  implicit val formatProteinIdent = Json.format[ProteinIdent]


  implicit val writeProteinMatchMultipleSearches = new Writes[ProteinMatchMultipleSearches] {
    override def writes(o: ProteinMatchMultipleSearches): JsValue = {
      JsObject(o.dict.map(acVal =>
        acVal._1.value -> JsObject(acVal._2.map(protId =>
            protId._1.value -> Json.toJson(protId._2)
          ).toSeq
        )
      ).toSeq
      )
    }
  }

  implicit val writesPepSpectraMatchWithSpectrumRef = new Writes[PepSpectraMatchWithSpectrumRef] {
    def writes(o: PepSpectraMatchWithSpectrumRef) =
    Json.toJson(o.asInstanceOf[PepSpectraMatch]).asInstanceOf[JsObject] ++ Json.obj("spectrumRef" -> Json.toJson(o.spectrumRef))
  }


  implicit def writesPepSpectraMatchWithSpectrumRefSeq = new Writes[Seq[PepSpectraMatchWithSpectrumRef]] {
    override def writes(o: Seq[PepSpectraMatchWithSpectrumRef]): JsValue = {
      JsArray(o.map(writesPepSpectraMatchWithSpectrumRef.writes))
    }
  }
}
