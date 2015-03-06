package ch.isbsib.proteomics.mzviz.modifications.services

import ch.isbsib.proteomics.mzviz.modifications.{ModifName}
import ch.isbsib.proteomics.mzviz.modifications.models.{Modification, PositionedModifRef}
import play.api.libs.json._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonModificationFormats {

  import play.api.libs.json.Json


  implicit val formatModifName = new Format[ModifName] {
    override def reads(json: JsValue): JsResult[ModifName] = JsSuccess(ModifName(json.as[String]))

    def writes(o: ModifName) = JsString(o.value)
  }

  implicit val formatModification = Json.format[Modification]
  implicit val formatPositionedModifRef = Json.format[PositionedModifRef]

}
