package ch.isbsib.proteomics.mzviz.controllers

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.IdRun
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, RefSpectrum}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.MatchMongoDBService
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action

/**
 * @author Alexandre Masselot
 */
@Api(value = "/matches", description = "PSM, SSM etc.")
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
          val idMatch = request.body.dataParts.get("id").map(_.head)
          val psms = LoaderMzIdent.parse(uploadedFile.getAbsolutePath)
          MatchMongoDBService().insert(psms)
      }
        .map { n => Ok(Json.obj("inserted" -> n))
      }.recover {
        case e => BadRequest(e.getMessage)
      }
  }

  @ApiOperation(nickname = "findExpSpectrum",
    value = "find a spectrum by run id and spectrum title",
    notes = """the tuple should be unique by indexing""",
    response = classOf[ExpMSnSpectrum],
    httpMethod = "GET")
  def findExpSpectrum(@ApiParam(value = """run id""", defaultValue = "") @PathParam("idRun") idRun: String,
                      @ApiParam(value = """spectrum title""", defaultValue = "") @PathParam("title") title: String) =
    Action.async {
      ExpMongoDBService().findSpectrumByRunIdAndTitle(IdRun(idRun), title)
        .map { case sp: ExpMSnSpectrum => Ok(Json.toJson(sp))}
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "findAllRefSpectraByIdRun",
    value = "find all spectra for a given run id",
    notes = """Returns only the reference information (precursor & co)""",
    response = classOf[List[RefSpectrum]],
    httpMethod = "GET")
  def findAllRefSpectraByIdRun(@ApiParam(value = """run id""", defaultValue = "") @PathParam("idRun") idRun: String) =
    Action.async {
      ExpMongoDBService().findAllRefSpectraByIdRun(IdRun(idRun))
        .map { case sphList: List[JsObject] => Ok(Json.toJson(sphList))}
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "deleteMSRun",
    value = "delete a run and all experimental data associated with it",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteMSRun(idRun: String) = Action.async {
    ExpMongoDBService().delete(IdRun(idRun)).map { x =>
      Ok("OK")
    }
  }
}
