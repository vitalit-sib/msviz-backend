package ch.isbsib.proteomics.mzviz.controllers.qc

import java.io.File
import java.text.SimpleDateFormat
import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.qc.QcDate
import ch.isbsib.proteomics.mzviz.qc.importer.LoadSummary
import ch.isbsib.proteomics.mzviz.qc.models.QcSummaryEntry
import ch.isbsib.proteomics.mzviz.qc.services.SummaryMongoDBServices
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import ch.isbsib.proteomics.mzviz.qc.services.JsonQCFormats._
import play.api.libs.json._
import com.wordnik.swagger.annotations._
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Action
import play.api.mvc.BodyParsers.parse

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by qinfang Jolliet on 12/08/15.
 */


@Api(value = "/qc", description = "Qc data access")
object QcController extends CommonController {

  @ApiOperation(nickname = "loadSummary",
    value = "Loads QcSummary file",
    notes = """ analyse Summary column's value by dates""",
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "QcSummary file", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadQcSummary() = Action.async(parse.temporaryFile) {
    request => val entries = LoadSummary(request.body.file).getSummaryEntry
      SummaryMongoDBServices().insert(entries).map{n => Ok(Json.obj("inserted" -> n))
      }.recover {
      case e => BadRequest(Json.toJson(e))
    }
  }

  @ApiOperation(nickname = "QcSummaryEntryListAll",
    value = "List all SummaryEntry",
    notes = """Returns only the QcSummaryEntry information """,
    response = classOf[Seq[QcSummaryEntry]],
    httpMethod = "GET")
  def listAll=
    Action.async {
      SummaryMongoDBServices().listAll
        .map { qcSummaryEntry => Ok(Json.toJson(qcSummaryEntry))
      }.recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "findQcSummaryEntryByDate",
    value = "find SummaryEntry by date",
    notes = """Returns only the QcSummaryEntry information """,
    response = classOf[Seq[QcSummaryEntry]],
    httpMethod = "GET")
  def findQcSummaryEntryByDate(@ApiParam(value = """QcDate""", defaultValue = "") @PathParam("Date") qcDate: String) =
    Action.async {
      SummaryMongoDBServices().findAllByDate(qcDate)
        .map { qcSummaryEntry => Ok(Json.toJson(qcSummaryEntry))
      }.recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "findQcSummaryEntryBtw2Date",
    value = "find SummaryEntry between two dates",
    notes = """Returns only the QcSummaryEntry information """,
    response = classOf[Seq[QcSummaryEntry]],
    httpMethod = "GET")
  def findQcSummaryBtw2Date(
                             @ApiParam(value = """QcDate1""", defaultValue = "") @PathParam("Date") qcDate1: String,
                               @ApiParam(value = """QcDate2""", defaultValue = "") @PathParam("Date") qcDate2: String) =
    Action.async {
      SummaryMongoDBServices().findAllBtw2Date(qcDate1,qcDate2)
        .map { qcSummaryEntry => Ok(Json.toJson(qcSummaryEntry))
      }.recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "deleteQcSummaryEntryByDate",
    value = "delete QcSummaryEntry data by Date",
    notes = """only can delete one day's data""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteQcSummaryByDate(qcDate: String) = Action.async {
    SummaryMongoDBServices().deleteAllByDate(qcDate).map { x =>
      Ok("OK")
    }
  }

}
