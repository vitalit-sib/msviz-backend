package ch.isbsib.proteomics.mzviz.controllers

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.commons.SpectraSource
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.IdRun
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.models.PepSpectraMatch
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.matches.services.MatchMongoDBService
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, Swiss Institute of Bioinformatics
 */
@Api(value = "/match", description = "PSMs, SSMs, protain matches etc.")
object MatchController extends CommonController {

  def stats = Action.async {
    ExpMongoDBService().stats.map { st =>
      Ok(jsonWritesMap.writes(st))
    }
  }

  @ApiOperation(nickname = "listMSRunIds",
    value = "the list of run ids",
    notes = """from the parameter run-id at load time""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def listMSRunIds = Action.async {
    ExpMongoDBService().listMsRunIds.map {
      ids => Ok(Json.obj("msRuns" -> ids.map(_.value)))
    }
  }

  @ApiOperation(nickname = "loadMzId",
    value = "Loads an mzid run",
    notes = """ """,
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "mzid", value = "mzid file", required = true, dataType = "file", paramType = "body")
  ))
  def loadPsms = Action.async(parse.multipartFormData) {
    request =>
      localFile("mzid", request)
        .flatMap {
        case (uploadedFile, filename) =>
          //          val idSearch = request.body.dataParts.get("search-id").map(_.head)
          val psms = LoaderMzIdent.parse(uploadedFile.getAbsolutePath)
          MatchMongoDBService().insert(psms)
      }
        .map { n => Ok(Json.obj("inserted" -> n))
      }.recover {
        case e => BadRequest(e.getMessage)
      }
  }

  @ApiOperation(nickname = "findAllPSMByRunId",
    value = "find all PSMs by by runId",
    notes = """PSMs list""",
    response = classOf[List[PepSpectraMatch]],
    httpMethod = "GET")
  def findAllPSMByRunId(
                         @ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String
                         ) =
    Action.async {
      MatchMongoDBService().findAllPSMByRunId(SpectraSource(runId))
        .map { case sphList: List[JsObject] => Ok(Json.toJson(sphList))}
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "deleteAllBySource",
    value = "delete PSMs for a given run-id",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteAllBySource(idRun: String) = Action.async {
    MatchMongoDBService().deleteAllBySource(SpectraSource(idRun)).map { x =>
      Ok("OK")
    }
  }
}
