package ch.isbsib.proteomics.mzviz.controllers.matches

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.controllers.matches.PSMController._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.SearchInfo
import ch.isbsib.proteomics.mzviz.matches.services.{MatchMongoDBService, SearchInfoDBService}
import com.wordnik.swagger.annotations._
import play.api.Logger
import play.api.cache.Cached
import play.api.libs.json._
import play.api.mvc.Action
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/searches", description = "searches")
object SearchController extends MatchController {


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

  @ApiOperation(nickname = "list",
    value = "the list of search ids",
    notes = """from the parameter search-id at load time""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def list =
    Action.async {
      MatchMongoDBService().listSearchIds.map {
        ids => Ok(Json.obj("searchIds" -> ids.map(_.value)))
      }
    }


  @ApiOperation(nickname = "get",
    value = "find all SearchInfo object for a given searchId",
    notes = """SearchInfos  list in TSV or JSON form""",
    response = classOf[List[SearchInfo]],
    produces = "application/json, application/tsv",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "searchId", value = "", required = false, dataType = "string", paramType = "searchId")
  ))
  def get(
           @ApiParam(value = """searchId""", defaultValue = "M_100") @PathParam("searchId") searchId: String
           ) = Cached(req => req.uri) {
    Action.async {
      for {
        searchInfo <- SearchInfoDBService().get(SearchId(searchId))
      } yield {
        Ok(Json.toJson(searchInfo))
      }

    }
  }

  @ApiOperation(nickname = "loadMzId",
    value = "Loads an mzid run, psms & searchInfo",
    notes = """ """,
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "mzid file", required = true, dataType = "application/xml", paramType = "body")
  ))
  def loadMzId(@ApiParam(name = "searchId", value = "a string id with search identifier") @PathParam("searchId") searchId: String,
               //@ApiParam(name = "runId", value = "a string id with run identifier (if not present, the searchId will be taken)", required = false)
               runId: Option[String]) =
    Action.async(parse.temporaryFile) {
      request =>
        val rid = runId match {
          case Some(r: String) => RunId(r)
          case None => RunId(searchId)
        }
        (for {
          psms <- Future {
            LoaderMzIdent.parse(request.body.file, SearchId(searchId), rid)
          }
          searchInfo <- Future {
            LoaderMzIdent.parseSearchInfo(request.body.file, SearchId(searchId))
          }
          n <- MatchMongoDBService().insert(psms)
          nInfo <- SearchInfoDBService().insert(searchInfo)
        } yield {
            Ok(Json.obj("psms" -> n, "searches" -> nInfo))
          }).recover {
          case e =>
            Logger.error(e.getMessage, e)
            BadRequest(Json.prettyPrint(Json.obj("status" -> "ERROR", "message" -> e.getMessage)) + "\n")
        }
    }

  @ApiOperation(nickname = "delete",
    value = "delete PSMs & searchInfo for a given list of searchIds (or one), seperated by comma",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def delete(@ApiParam(value = """searchIds""", defaultValue = "") @PathParam("searchIds") searchIds: String) = Action.async {
    MatchMongoDBService().deleteAllBySearchId(queryParamSearchIds(searchIds)).map {
      x =>
        Ok("OK")
    }
  }
}
