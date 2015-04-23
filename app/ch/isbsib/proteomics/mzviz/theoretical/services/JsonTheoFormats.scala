package ch.isbsib.proteomics.mzviz.theoretical.services

import ch.isbsib.proteomics.mzviz.matches.models.{SearchInfo, ProteinRef}
import ch.isbsib.proteomics.mzviz.theoretical.models.{SequenceSourceStats, FastaEntry}
import ch.isbsib.proteomics.mzviz.theoretical.{ProteinIdentifier, AccessionCode, SequenceSource}
import play.api.libs.json._

/**
 * Created by tmartinc on 05/12/14.
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonTheoFormats {

  import play.api.libs.json.Json

  implicit val formatProteinIdentifier = new Format[ProteinIdentifier] {
    override def reads(json: JsValue): JsResult[ProteinIdentifier] = JsSuccess(ProteinIdentifier(json.as[String]))

    def writes(o: ProteinIdentifier) = JsString(o.value)
  }

  implicit val formatAccessionCode = new Format[AccessionCode] {
    override def reads(json: JsValue): JsResult[AccessionCode] = JsSuccess(AccessionCode(json.as[String]))

    def writes(o: AccessionCode) = JsString(o.value)
  }

  implicit val formatSequenceSource = new Format[SequenceSource] {
    override def reads(json: JsValue): JsResult[SequenceSource] = JsSuccess(SequenceSource(json.as[String]))

    def writes(o: SequenceSource) = JsString(o.value)
  }

  implicit val formatProteinRef = Json.format[ProteinRef]

  implicit val jsonSequenceSource = new Writes[SequenceSource] {
    override def writes(o: SequenceSource): JsValue = {
      JsString(o.value)
    }
  }

  implicit val jsonSequenceSourceStats = new Writes[SequenceSourceStats] {
    override def writes(o: SequenceSourceStats): JsValue = Json.obj(
      "source" -> o.source,
      "nbEntries" -> o.nbEntries,
      "nbResidues" -> o.nbResidues
    )
  }


  implicit val jsonSequenceSourceStatsMap = new Writes[Map[SequenceSource, SequenceSourceStats]] {
    override def writes(o: Map[SequenceSource, SequenceSourceStats]): JsValue = {
      o.foldLeft(Json.obj())({ (acc, p) => acc ++ Json.obj(p._1.value.toString -> Json.toJson(p._2))})
    }
  }

  implicit val jsonProteinRef = new Writes[ProteinRef] {
    override def writes(o: ProteinRef): JsValue = Json.obj(
      "AC" -> o.AC,
      "source" -> o.source.get
    )
  }

  implicit val formatFastaEntry:Format[FastaEntry] = Json.format[FastaEntry]

}
