package ch.isbsib.proteomics.mzviz.controllers.matches

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.SearchInfo
import ch.isbsib.proteomics.mzviz.matches.services.{MatchMongoDBService, SearchInfoDBService}
import com.wordnik.swagger.annotations._
import play.api.cache.Cached
import play.api.libs.json._
import play.api.mvc.Action
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

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

  @ApiOperation(nickname = "listSearchIds",
    value = "the list of search ids",
    notes = """from the parameter search-id at load time""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def listSearchIds =
    Action

      .async {
      MatchMongoDBService().listSearchIds.map {
        ids => Ok(Json.obj("searchIds" -> ids.map(_.value)))
      }
    }


  @ApiOperation(nickname = "findAllSearchInfoBySearchId",
    value = "find all SearchInfo object for a given searchId",
    notes = """SearchInfos  list in TSV or JSON form""",
    response = classOf[List[SearchInfo]],
    produces = "application/json, application/tsv",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "searchId", value = "", required = false, dataType = "string", paramType = "searchId")
  ))
  def findAllSearchInfoBySearchId(
                                   @ApiParam(value = """searchId""", defaultValue = "M_100") @PathParam("searchId") searchId: String
                                   ) = Cached(req => req.uri) {
    Action.async {
      for {
        searchInfo <- SearchInfoDBService().findAllSearchInfoBySearchId(SearchId(searchId))
      } yield {
        Ok(Json.toJson(searchInfo))
      }

    }
  }

}
