package ch.isbsib.proteomics.mzviz.controllers.theoretical

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import ch.isbsib.proteomics.mzviz.theoretical.{ProteinIdentifier, SequenceSource}
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action


/**
 *
 * handles sequence REST API
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/sequence", description = "sequence access etc.")
object OneSequenceController extends CommonController {


  @ApiOperation(nickname = "get",
    value = "get one entry",
    notes = """from source and identifier (AC or ID)""",
    response = classOf[JsValue],
    httpMethod = "GET")
  def get(
           @ApiParam(value = """sourceId""", defaultValue = "uniprot_sprot_20231224") @PathParam("sourceId") sourceId: String,
           @ApiParam(value = """identifier""", defaultValue = "Some identifier") @PathParam("identifier") identifier: String
           ) =
    Action.async(parse.temporaryFile) {
      request =>
        SequenceMongoDBService().findEntryByIdentifierAndSources(ProteinIdentifier(identifier), SequenceSource(sourceId))
          .map { e =>
          Ok(Json.toJson(e))
        }.recover {
          case e => BadRequest(Json.toJson(e))
        }
    }

}
