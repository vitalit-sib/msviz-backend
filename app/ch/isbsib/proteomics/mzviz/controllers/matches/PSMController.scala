package ch.isbsib.proteomics.mzviz.controllers.matches

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.spectrasim.services.JsonSimFormats._
import ch.isbsib.proteomics.mzviz.controllers.TsvFormats
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.models.{PepSpectraMatch, ProteinRef, SearchInfo}
import ch.isbsib.proteomics.mzviz.matches.services.{MatchMongoDBService, SearchInfoDBService}
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpRefMatch
import ch.isbsib.proteomics.mzviz.spectrasim.services.SimilarSpectraMongoDBService
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import com.wordnik.swagger.annotations._
import play.api.Logger
import play.api.cache.Cached
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/match", description = "PSMs, SSMs, protein matches etc.")
object PSMController extends MatchController {


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

  @ApiOperation(nickname = "findAllProteinRefsBySearchId",
    value = "find all protein for a given searchId",
    notes = """ProteinRef list""",
    response = classOf[List[ProteinRef]],
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "withModif", value = "modification name", required = false, dataType = "String", paramType = "query")
  ))
  def findAllProteinRefsBySearchIds(
                                    @ApiParam(value = """searchIds""", defaultValue = "M_100") @PathParam("searchIds") searchIds: String,
                                    withModif:Option[String]
                                    ) =  Cached(req => req.uri) {
    Action.async {
      for {
        protRefs <- MatchMongoDBService().listProteinRefsBySearchIds(queryParamSearchIds(searchIds), queryParamOModifName(withModif))
      } yield {
        Ok(Json.toJson(protRefs))
      }

    }
  }

  @ApiOperation(nickname = "findAllModificationsBySearchIds",
    value = "find all unique modification for a list of searchIds",
    notes = """modification name lis""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def findAllModificationsBySearchIds(
                                    @ApiParam(value = """searchIds""", defaultValue = "M_100") @PathParam("searchIds") searchIds: String
                                    ) =  Cached(req => req.uri) {
    val sids = queryParamSearchIds(searchIds)
    Action.async {
      for {
        modifNames <- MatchMongoDBService().findAllModificationsBySearchIds(sids)
      } yield {
        Ok(Json.toJson(modifNames))
      }

    }
  }

  @ApiOperation(nickname = "findSimilarSpectra",
    value = "find similar spectra for a given run id and spectrum title",
    notes = """Returns spectra matches""",
    response = classOf[List[SpSpRefMatch]],
    httpMethod = "GET")
  def findSimilarSpectra(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String,
                         @ApiParam(value = """spectrum title""", defaultValue = "") @PathParam("title") title: String,
                         @ApiParam(value = """score threshold""", defaultValue = "") @PathParam("scoreThreshold") scoreThreshold: String,
                         @ApiParam(value = """tolerance in Dalton to match ms2 peaks""", defaultValue = "") @PathParam("ms2PeakMatchTol") ms2PeakMatchTol: String) =
    Cached(req => req.uri) {
      Action.async {
        for {
          spMatches <- SimilarSpectraMongoDBService().findSimSpMatches(RunId(runId), title, scoreThreshold.toDouble, ms2PeakMatchTol.toDouble)
        } yield {
          Ok(Json.toJson(spMatches.toList))
        }

      }
    }


  @ApiOperation(nickname = "findAllPSMByProteinAC",
    value = "find all PSMs object for a given protein",
    notes = """PSMs  list in TSV or JSON form""",
    response = classOf[List[PepSpectraMatch]],
    produces = "application/json, application/tsv",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "sequenceSource", value = "", required = false, dataType = "string", paramType = "sequenceSource")
  ))
  def findAllPSMByProteinAC(
                        @ApiParam(value = """searchIds""", defaultValue = "") @PathParam("searchIds") searchIds: String,
                        @ApiParam(value = """accessionCode""", defaultValue = "") @PathParam("accessionCode") accessionCode: String,
                        sequenceSource: Option[String]
                        ) =
    Cached(req => {
      req.uri+"/Accept="+req.headers.get("Accept")
    }) {
      Action.async { implicit request =>
        MatchMongoDBService().findPSMByProtein(
          AccessionCode(accessionCode),
          source = sequenceSource.map(s => SequenceSource(s)),
          searchIds = if (searchIds == "*") None else Some(searchIds.split(",").toList.map(s => SearchId(s)).toSet)
        )
          .map { case psms =>
          render {
            //case acceptsTsv() => Ok(TsvFormats.toTsv(psms.map(_.extractAC(AccessionCode(accessionCode))), showFirstProtMatchInfo = true))
            case _ => Ok(Json.toJson(psms))
          }
        }
          .recover {
          case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }
      }
    }


}
