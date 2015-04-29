package ch.isbsib.proteomics.mzviz.controllers

import javax.ws.rs.PathParam
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.models.{ProteinRef, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.matches.services.SearchInfoDBService
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpRefMatch
import ch.isbsib.proteomics.mzviz.spectrasim.services.{SimilarSpectraMongoDBService}
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import ch.isbsib.proteomics.mzviz.theoretical.models.SequenceSourceStats
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.spectrasim.services.JsonSimFormats._
import ch.isbsib.proteomics.mzviz.matches.services.{SearchInfoDBService, MatchMongoDBService}
import com.wordnik.swagger.annotations._
import play.api.Logger
import play.api.Play.current
import play.api.cache.Cached
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action
import reactivemongo.bson.BSONDocument

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
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


  @ApiOperation(nickname = "loadMzId",
    value = "Loads an mzid run",
    notes = """ """,
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "mzid file", required = true, dataType = "application/xml", paramType = "body")
  ))
  def loadPsms(@ApiParam(name = "searchId", value = "a string id with search identifier") @PathParam("searchId") searchId: String,
               //@ApiParam(name = "runId", value = "a string id with run identifier (if not present, the searchId will be taken)", required = false)
               runId: Option[String]) =
    Cached(req => req.uri) {
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
            nInfo<-SearchInfoDBService().insert(searchInfo)
          } yield {
              Ok(Json.obj("inserted" -> n, "searchInfoInserted" -> nInfo))
            }).recover {
            case e =>
              Logger.error(e.getMessage, e)
              BadRequest(Json.prettyPrint(Json.obj("status" -> "ERROR", "message" -> e.getMessage)) + "\n")
          }
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
  def findAllProteinRefsBySearchId(
                                    @ApiParam(value = """searchId""", defaultValue = "M_100") @PathParam("searchId") searchId: String
                                    ) =  Cached(req => req.uri) {
    Action.async {
      for {
        protRefs <- MatchMongoDBService().listProteinRefsBySearchId(SearchId(searchId))
      } yield {
        Ok(Json.toJson(protRefs))
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


  @ApiOperation(nickname = "findPSMByProtein",
    value = "find all PSMs object for a given protein",
    notes = """PSMs  list in TSV or JSON form""",
    response = classOf[List[PepSpectraMatch]],
    produces = "application/json, application/tsv",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "sequenceSource", value = "", required = false, dataType = "string", paramType = "sequenceSource")
  ))
  def findPSMByProtein(
                        @ApiParam(value = """searchIds""", defaultValue = "") @PathParam("searchIds") searchIds: String,
                        @ApiParam(value = """accessionCode""", defaultValue = "") @PathParam("accessionCode") accessionCode: String,
                        sequenceSource: Option[String]
                        ) =
    Cached(req => req.uri) {
      Action.async { implicit request =>
        MatchMongoDBService().findPSMByProtein(
          AccessionCode(accessionCode),
          source = sequenceSource.map(s => SequenceSource(s)),
          searchIds = if (searchIds == "*") None else Some(searchIds.split(",").toList.map(s => SearchId(s)).toSet)
        )
          .map { case psms =>
          render {
            case acceptsTsv() => Ok(TsvFormats.toTsv(psms.map(_.extractAC(AccessionCode(accessionCode))), showFirstProtMatchInfo = true))
            case _ => Ok(Json.toJson(psms))
          }
        }
          .recover {
          case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }
      }
    }

  @ApiOperation(nickname = "deleteAllByRunId",
    value = "delete PSMs for a given list of searchIds (or one), seperated by comma",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteAllBySearchId(@ApiParam(value = """searchIds""", defaultValue = "") @PathParam("searchIds") searchIds: String) = Action.async {
    val s = searchIds.split(",").toList.map(SearchId.apply)
    MatchMongoDBService().deleteAllBySearchId(s).map {
      x =>
        Ok("OK")
    }
  }
}
