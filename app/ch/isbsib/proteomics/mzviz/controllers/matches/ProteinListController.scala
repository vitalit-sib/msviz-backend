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
import ch.isbsib.proteomics.mzviz.matches.services.{ProteinListMongoDBService, MatchMongoDBService, SearchInfoDBService}
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
object ProteinListController extends MatchController {


  def stats = Action.async {
    ExpMongoDBService().stats.map { st =>
      Ok(jsonWritesMap.writes(st))
    }
  }

  @ApiOperation(nickname = "findAllPSMByRunId",
    value = "find all PSMs by searchId",
    notes = """PSMs list""",
    response = classOf[List[PepSpectraMatch]],
    httpMethod = "GET")
  def findAllPSMBySearchId(
                            @ApiParam(value = """searchId""", defaultValue = "") @PathParam("searchId") searchId: String
                            ) =
    Action.async { implicit request =>
      MatchMongoDBService().findAllPSMBySearchId(SearchId(searchId))
        .map { case sphList =>
        render {
          case acceptsTsv() => Ok(TsvFormats.toTsv(sphList))
          case _ => Ok(Json.toJson(sphList))
        }
      }
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

}
