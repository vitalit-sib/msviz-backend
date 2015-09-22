package ch.isbsib.proteomics.mzviz.controllers.matches

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.matches.PSMController._
import ch.isbsib.proteomics.mzviz.controllers.matches.ProteinMatchController._
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.ProteinMatchMultipleSearches
import ch.isbsib.proteomics.mzviz.matches.services.ProteinMatchMongoDBService
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import com.wordnik.swagger.annotations._
import play.api.cache.Cached
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/proteinListMultipleSearches", description = "list of identified proteins for all given searches")
object ProteinMatchMultipleSearchesController extends MatchController {


  @ApiOperation(nickname = "findAllProteinsForMultipleSearchIds",
    value = "find all proteins corresponding to given searchIds",
    notes = """protein list""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def findAllProteinsForMultipleSearchIds(
                                           @ApiParam(value = """searchIds""") @PathParam("searchIds") searchIds: String
                                           ) = Cached(req => req.uri) {
    Action.async {
      val sids = queryParamSearchIds(searchIds)

      for {
        proteinMultipleList <- ProteinMatchMongoDBService().findAllProteinsBySearchIds(sids).map(
          _.foldLeft( ProteinMatchMultipleSearches(Map()) )( (r, c) => r.add(c.searchId, c) )
        )
      } yield {
        Ok(Json.toJson(proteinMultipleList))
      }

    }
  }
  @ApiOperation(nickname = "findAllProteinsForMultipleSearchIdsAndACs",
    value = "find all proteins corresponding to given searchIds and Acs",
    notes = """protein list""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def findAllProteinsForMultipleSearchIdsAndACs(
                                           @ApiParam(value = """searchIds""") @PathParam("searchIds") searchIds: String,
                                           @ApiParam(value = """ACs""") @PathParam("ACs") accessionCodes: String
                                           ) = Cached(req => req.uri) {
    Action.async {
      val sids = queryParamSearchIds(searchIds)
      val acs=accessionCodes.split(",").toList.map(AccessionCode.apply).toSet
      for {
        proteinMultipleList <- ProteinMatchMongoDBService().findAllProteinsBySearchIdsAndACs(sids,acs).map(
          _.foldLeft( ProteinMatchMultipleSearches(Map()) )( (r, c) => r.add(c.searchId, c) )
        )
      } yield {
        Ok(Json.toJson(proteinMultipleList))
      }

    }
  }

}

