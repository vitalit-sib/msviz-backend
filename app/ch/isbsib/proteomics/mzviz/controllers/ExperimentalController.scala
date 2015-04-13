package ch.isbsib.proteomics.mzviz.controllers

import java.io.File
import javax.ws.rs.PathParam

import JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumRef, ExpMSnSpectrum}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpMatch
import com.wordnik.swagger.annotations._
import play.api.libs.Files
import play.api.mvc.{MultipartFormData, Request, Action, Controller}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

import reactivemongo.api._

import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/exp", description = "experimental data access")
object ExperimentalController extends CommonController {

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
      ids => Ok(Json.toJson(ids.map(_.value)))
    }
  }

  @ApiOperation(nickname = "loadMSRun",
    value = "Loads an MGF peak list as a run",
    notes = """ runId will be important to link with the mzid""",
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "mgf peak list", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadMSRun(@ApiParam(name = "runId", value = "a string id with run identifier", required = true) @PathParam("runId") runId: String) = Action.async(parse.temporaryFile) {
    request =>

      LoaderMGF.load(request.body.file, RunId(runId)) match {
        case Success(msRun) => ExpMongoDBService().insert(msRun)
          .map { n => Ok(Json.obj("inserted" -> n))
        }.recover {
          case e => BadRequest(e.getMessage)
        }
        case Failure(e) => Future {
          BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }
      }

  }

  @ApiOperation(nickname = "findExpSpectrum",
    value = "find a spectrum by run id and spectrum title",
    notes = """the tuple should be unique by indexing""",
    response = classOf[ExpMSnSpectrum],
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "sortByMoz", value = "sort the fragment peaks by m/z (defaukt false)", required = false, dataType = "Boolean", paramType = "query"),
    new ApiImplicitParam(name = "mostIntense", value = "take the n most intense peaks", required = false, dataType = "Integer", paramType = "query")
  ))
  def findExpSpectrum(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String,
                      @ApiParam(value = """spectrum title""", defaultValue = "") @PathParam("title") title: String,
                      sortByMoz: Option[Boolean]=None,
                      mostIntense: Option[Integer]=None
                       ) =
    Action.async {
      println(sortByMoz, mostIntense)
      ExpMongoDBService().findSpectrumByRunIdAndTitle(RunId(runId), title)
        .map { case sp: ExpMSnSpectrum =>
        val peaks = (sortByMoz match {
          case Some(false) => sp.peaks
          case _ => sp.peaks.sortBy(_.moz.value)
        }).filter({ p =>
          mostIntense match {
            case None => true
            case Some(thres) =>
              p.intensityRank.value <= thres
          }
        })
          Ok(Json.toJson(ExpMSnSpectrum(sp.ref,peaks)))
      }
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "findAllSpectraRefByRunId",
    value = "find all spectra for a given run id",
    notes = """Returns only the reference information (precursor & co)""",
    response = classOf[List[SpectrumRef]],
    httpMethod = "GET")
  def findAllSpectraRefByRunId(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String) =
    Action.async {
      ExpMongoDBService().findAllSpectraRefByrunId(RunId(runId))
        .map { case sphList: List[JsObject] => Ok(Json.toJson(sphList)) }
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "deleteMSRun",
    value = "delete a run and all experimental data associated with it",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteMSRun(runId: String) = Action.async {
    ExpMongoDBService().delete(RunId(runId)).map { x =>
      Ok("OK")
    }
  }
}
