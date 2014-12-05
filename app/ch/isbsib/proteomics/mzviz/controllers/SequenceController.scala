package ch.isbsib.proteomics.mzviz.controllers

import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.theoretical.SequenceSource
import ch.isbsib.proteomics.mzviz.theoretical.importer.FastaParser
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action


/**
 *
 * handles sequence REST API
 * @author Alexandre Masselot
 */
@Api(value = "/sequence", description = "sequence access etc.")
object SequenceController extends CommonController {

  def stats = Action.async {
    SequenceMongoDBService().stats.map { st =>
      Ok(jsonWritesMap.writes(st))
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
    new ApiImplicitParam(name = "fasta", value = "fasta file", required = true, dataType = "file", paramType = "body"),
    new ApiImplicitParam(name = "source", value = "a string id with source/version", required = true, dataType = "string", paramType = "body")
  ))
  def loadFasta = Action.async(parse.multipartFormData) {
    request =>
      localFile("fasta", request)
        .flatMap {
        case (uploadedFile, filename) =>
          val src = request.body.dataParts.get("source").map(_.head).get
          val entries = FastaParser(uploadedFile.getAbsolutePath, SequenceSource(src)).parse
          SequenceMongoDBService().insert(entries)
      }
        .map { n => Ok(Json.obj("inserted" -> n))
      }.recover {
        case e => BadRequest(Json.toJson(e))
      }
  }
}
