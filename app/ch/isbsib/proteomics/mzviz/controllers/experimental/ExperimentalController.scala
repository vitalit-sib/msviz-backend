package ch.isbsib.proteomics.mzviz.controllers.experimental

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.commons.Moz
import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, SpectrumRef}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMs1MongoDBService, ExpMongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action

import scala.concurrent.Future
import scala.util.{Failure, Success}

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


  @ApiOperation(nickname = "findXIC",
    value = "find all ms1 for a given run id and moz",
    notes = """Returns only list of retention times and intensities""",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "tolerance", value = "tolerance", required = false, dataType = "Double", paramType = "query")
  ))
  def findXic(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String,
              @ApiParam(value = """m/z""", defaultValue = "") @PathParam("moz") moz: Double,
              tolerance: Option[Double]=None
               ) =
    Action.async {
     val futureList= ExpMs1MongoDBService().findMs1ByRunID_MozAndTol(RunId(runId),Moz(moz),tolerance.getOrElse(0.01))
      ExpMs1MongoDBService().extract2Lists(futureList)
      .map { case sphList: JsObject => Ok(sphList) }
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

}
