package ch.isbsib.proteomics.mzviz.controllers

import javax.ws.rs.PathParam
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.models.{ProteinRef, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import ch.isbsib.proteomics.mzviz.theoretical.models.SequenceSourceStats
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.matches.services.MatchMongoDBService
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action
import reactivemongo.bson.BSONDocument

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, Swiss Institute of Bioinformatics
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
  def listSearchIds = Action.async {
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
    new ApiImplicitParam(name = "mzid", value = "mzid file", required = true, dataType = "file", paramType = "body"),
    new ApiImplicitParam(name = "searchId", value = "a string id with search identifier", required = true, dataType = "string", paramType = "body"),
    new ApiImplicitParam(name = "runId", value = "a string id with run identifier", required = true, dataType = "string", paramType = "body")
  ))
  def loadPsms = Action.async(parse.multipartFormData) {
    request =>
      localFile("mzid", request)
        .flatMap {
        case (uploadedFile, filename) =>
          val searchId = request.body.dataParts.get("searchId").map(_.head).get
          val runId = request.body.dataParts.get("runId").map(_.head).get
          val psms = LoaderMzIdent.parse(uploadedFile.getAbsolutePath, SearchId(searchId), RunId(runId))
          MatchMongoDBService().insert(psms)
      }
        .map { n => Ok(Json.obj("inserted" -> n))
      }.recover {
        case e => BadRequest(e.getMessage)
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
    Action.async {
      MatchMongoDBService().findAllPSMBySearchId(SearchId(searchId))
        .map { case sphList => Ok(Json.toJson(sphList))}
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "findProteinsBySearchId",
    value = "find all protein for a given searchId",
    notes = """ProteinRef list""",
    response = classOf[List[ProteinRef]],
    httpMethod = "GET")
  def findAllProteinRefsBySearchId(
                                    @ApiParam(value = """searchId""", defaultValue = "M_100") @PathParam("searchId") searchId: String
                                    ) = Action.async {
    for {
      protRefs <- MatchMongoDBService().listProteinRefsBySearchId(SearchId(searchId))
    } yield {
      Ok(Json.toJson(protRefs))
    }

  }

  @ApiOperation(nickname = "findPSMByProtein",
    value = "find all PSMs object for a given protein",
    notes = """PSMs like list (there is no list of the protein, but just a pointer to the one asked""",
    response = classOf[List[JsObject]],
    httpMethod = "GET")
  def findPSMByProtein(
                        @ApiParam(value = """searchId""", defaultValue = "") @PathParam("searchId") searchId: String,
                        @ApiParam(value = """sequenceSource""", defaultValue = "") @PathParam("sequenceSource") sequenceSource: String,
                        @ApiParam(value = """accessionCode""", defaultValue = "") @PathParam("accessionCode") accessionCode: String
                        ) =
    Action.async {
      MatchMongoDBService().findPSMByProtein(SearchId(searchId), SequenceSource(sequenceSource), AccessionCode(accessionCode))
        .map { case psms=> Ok(psms)} //Ok(sphList)}
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }


  @ApiOperation(nickname = "deleteAllByRunId",
    value = "delete PSMs for a given searchId",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteAllBySearchId(searchId: String) = Action.async {
    MatchMongoDBService().deleteAllBySearchId(SearchId(searchId)).map { x =>
      Ok("OK")
    }
  }
}
