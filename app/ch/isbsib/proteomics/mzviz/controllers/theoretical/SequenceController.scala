package ch.isbsib.proteomics.mzviz.controllers.theoretical

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.theoretical.SequenceSource
import ch.isbsib.proteomics.mzviz.theoretical.importer.FastaParser
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import ch.isbsib.proteomics.mzviz.uploads.LoaderMQData
import com.wordnik.swagger.annotations._
import play.api.Play
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action


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

  @ApiOperation(nickname = "optionsId",
    value = "empty options method",
    notes = """returns Ok to fulfill the pre-flight OPTIONS request""",
    response = classOf[String],
    httpMethod = "OPTIONS")
  def optionsId(@ApiParam(value = """id""") @PathParam("id") id: String) =
    Action {
      Ok("Okay")
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

  @ApiOperation(nickname = "deleteSource",
    value = "delete all entries from a given source",
    notes = """use at your own risk :)""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteSource(@ApiParam(value = """sourceId""", defaultValue = "") @PathParam("sourceId") sourceId: String) =
    Action.async {
      SequenceMongoDBService().deleteAllBySource(SequenceSource(sourceId))
        .map {
        _ => Ok(Json.prettyPrint(
          Json.obj(
            "deleted" -> sourceId
          )))
      }.recover {
        case e => BadRequest(Json.toJson(e))
      }
    }

  @ApiOperation(nickname = "loadFasta",
    value = "Loads a fasta file",
    notes = """ source will be a unique descriptor on the source """,
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "fasta file", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadFasta(@ApiParam(value = """sourceId""", defaultValue = "uniprot_sprot_20231224") @PathParam("sourceId") sourceId: String,
                @ApiParam(value = """regexp""", defaultValue = "None") @PathParam("regexp") regexp:Option[String]=None) =
    Action.async(parse.temporaryFile) {
      request =>
        val entries = FastaParser(request.body.file, SequenceSource(sourceId), regexp).parse
        SequenceMongoDBService().insert(entries).map { n => Ok(Json.obj("inserted" -> n))
        }.recover {
          case e => BadRequest(Json.toJson(e))
        }
    }


  @ApiOperation(nickname = "loadFastaLocal",
    value = "Loads a fasta file from the local filesystem",
    notes = """ source will be a unique descriptor on the source """,
    response = classOf[String],
    httpMethod = "POST")
  def loadFastaLocal(@ApiParam(value = """sourceId""", defaultValue = "uniprot_sprot_20231224") @PathParam("sourceId") sourceId: String,
                @ApiParam(value = """regexp""", defaultValue = "None") @PathParam("regexp") regexp:Option[String]=None,
                     @ApiParam(value = """filename""", defaultValue = "None") @PathParam("fasta filename") filename:String) =
    Action.async {
        val pathFolder = Play.current.configuration.getString("upload.dir").get.toString
        val blockSize = Play.current.configuration.getString("theoretical.block.size").get.toInt

        val entries = FastaParser(pathFolder+filename, SequenceSource(sourceId), regexp).parse
        SequenceMongoDBService().insertSequentially(entries, blockSize).map { n => Ok(Json.obj("inserted" -> n))
        }.recover {
          case e => BadRequest(Json.toJson(e))
        }
    }

}
