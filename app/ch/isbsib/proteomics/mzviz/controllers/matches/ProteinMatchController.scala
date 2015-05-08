package ch.isbsib.proteomics.mzviz.controllers.matches

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.controllers.TsvFormats
import ch.isbsib.proteomics.mzviz.controllers.matches.PSMController._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{PepSpectraMatch, SearchInfo}
import ch.isbsib.proteomics.mzviz.matches.services.{ProteinMatchMongoDBService, MatchMongoDBService, SearchInfoDBService}
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
@Api(value = "/proteinList", description = "list of identified proteins")
object ProteinMatchController extends MatchController {


  @ApiOperation(nickname = "findAllProteinsBySearchId",
    value = "find all proteins corresponding to a given searchId",
    notes = """protein list""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def findAllProteinsBySearchIds(
                                       @ApiParam(value = """searchIds""") @PathParam("searchIds") searchIds: String
                                       ) =  Cached(req => req.uri) {
    Action.async {
      val sids = queryParamSearchIds(searchIds)
      for {
        proteinList <- ProteinMatchMongoDBService().findAllProteinsBySearchIds(sids)
      } yield {
        Ok(Json.toJson(proteinList))
      }

    }
  }


}
