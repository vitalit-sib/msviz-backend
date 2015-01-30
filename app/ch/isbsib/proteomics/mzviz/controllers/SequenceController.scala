package ch.isbsib.proteomics.mzviz.controllers

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import ch.isbsib.proteomics.mzviz.theoretical.importer.FastaParser
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._


/**
 *
 * handles sequence REST API
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/sequences", description = "sequence access etc.")
object SequenceController extends CommonController {

  def stats = Action.async {
    SequenceMongoDBService().stats.map { st =>
      Ok(Json.toJson(st))
    }
  }

  @ApiOperation(nickname = "listSources",
    value = "the list of sequence sources",
    notes = """from the parameter source at load time""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def listSources = Action.async {
    SequenceMongoDBService().listSources
      .map {
      srcs => Ok(Json.toJson(srcs))
    }.recover {
      case e => BadRequest(Json.toJson(e))
    }
  }


  @ApiOperation(nickname = "loadFasta",
    value = "Loads a fasta file",
    notes = """ source will be a unique descriptor on the source""",
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "fasta", value = "fasta file", required = true, dataType = "file", paramType = "body")
  ))
  def loadFasta(@ApiParam(value = """sourceId""", defaultValue = "uniprot_sprot_20231224") @PathParam("sourceId") sourceId: String) =
    Action.async(parse.temporaryFile) {
      request =>
        val entries = FastaParser(request.body.file, SequenceSource(sourceId)).parse
        SequenceMongoDBService().insert(entries).map { n => Ok(Json.obj("inserted" -> n))
        }.recover {
          case e => BadRequest(Json.toJson(e))
        }
    }
}
