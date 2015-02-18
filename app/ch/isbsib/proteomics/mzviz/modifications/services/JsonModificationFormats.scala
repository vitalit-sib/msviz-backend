package ch.isbsib.proteomics.mzviz.modifications.services

import ch.isbsib.proteomics.mzviz.modifications.{ModifSource, ModifAC}
import ch.isbsib.proteomics.mzviz.modifications.models.{Modification, PositionedModif, ModificationRef}
import play.api.libs.json._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonModificationFormats {

  import play.api.libs.json.Json

  implicit val formatModifAC = new Format[ModifAC] {
    override def reads(json: JsValue): JsResult[ModifAC] = JsSuccess(ModifAC(json.as[String]))

    def writes(o: ModifAC) = JsString(o.value)
  }

  implicit val formatModifSource = new Format[ModifSource] {
    override def reads(json: JsValue): JsResult[ModifSource] = JsSuccess(ModifSource(json.as[String]))

    def writes(o: ModifSource) = JsString(o.value)
  }

  implicit val formatModificationRef = Json.format[ModificationRef]
  implicit val formatModification = Json.format[Modification]
  implicit val formatPositionedModif = Json.format[PositionedModif]

}
