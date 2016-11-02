package ch.isbsib.proteomics.mzviz.controllers.results

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.matches.SearchController._
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.results.basket.BasketMongoDBService
import ch.isbsib.proteomics.mzviz.results.basket.models.{BasketEntryWithSpInfo, BasketEntry}
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import play.api.mvc.{Accepting, Action}
import play.api.libs.concurrent.Execution.Implicits._
import ch.isbsib.proteomics.mzviz.results.basket.JsonBasketFormats._
import play.api.mvc._
import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.results.basket.BasketTsvFormat

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/basket", description = "basket")
object BasketController extends CommonController{

  @ApiOperation(nickname = "put",
    value = "put basket entry",
    notes = """put a new basket entry or update an existing one""",
    response = classOf[Int],
    httpMethod = "PUT")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "entry", required = true, dataType = "application/json", paramType = "body")
  ))
  def put =
    Action.async {
      request =>
        val newElement: Seq[BasketEntry] = request.body.asText match {
          case Some(s) => Seq(Json.parse(s).as[BasketEntry])
          case None => Seq()
        }
        BasketMongoDBService().insertOrUpdate(newElement).map(inserted => Ok(Json.obj("nrInserted" ->inserted)))
  }

  @ApiOperation(nickname = "options",
    value = "empty options method",
    notes = """returns Ok to fulfill the pre-flight OPTIONS request""",
    response = classOf[String],
    httpMethod = "OPTIONS")
  def options =
    Action {
      Ok("Ok")
    }


  @ApiOperation(nickname = "optionsId",
    value = "empty options method",
    notes = """returns Ok to fulfill the pre-flight OPTIONS request""",
    response = classOf[String],
    httpMethod = "OPTIONS")
  def optionsId(@ApiParam(value = """id""") @PathParam("id") id: String) =
    Action {
      Ok("Ok")
    }

  @ApiOperation(nickname = "list-searchIds",
    value = "list the available searchIds",
    notes = """returns the list of searchIds""",
    response = classOf[Seq[String]],
    httpMethod = "GET")
  def listSearchIds =
    Action.async {
      for {
        searchIds <- BasketMongoDBService().listSearchIds
      } yield {
        Ok(Json.toJson(searchIds))
      }
    }

  @ApiOperation(nickname = "find-by-searchId",
    value = "get all entries corresponding to a certain searchId",
    notes = """returns the list of BasketEntries""",
    response = classOf[Seq[BasketEntryWithSpInfo]],
    produces = "application/json, application/tsv",
    httpMethod = "GET")
  def findBySearchId(@ApiParam(value = """searchId""") @PathParam("searchId") searchId: String) =
    Action.async { implicit request =>
      BasketMongoDBService().findBySearchIdWithSpInfo(searchId)
        .map { case basketEntries =>
          render {
            case Accepts.Json() => Ok(Json.toJson(basketEntries))
            case acceptsTsv() => Ok(BasketTsvFormat.toTsv(basketEntries))
          }
        }
        .recover {
          case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }

    }

  @ApiOperation(nickname = "deleteByMongoId",
    value = "delete a basket entry by MongoId",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteByMongoId(@ApiParam(value = """mongoId""", defaultValue = "") @PathParam("mongoId") mongoId: String) =
    Action.async { implicit request =>
      BasketMongoDBService().deleteByMongoId(mongoId).map {case basketEntries =>
        render {
          case Accepts.Json() => Ok(Json.toJson(basketEntries))
        }
      }.recover {
          case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }

    }


  @ApiOperation(nickname = "deleteBySearchIds",
    value = "delete a basket entry by MongoId",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteByBasketId(@ApiParam(value = """basketId""", defaultValue = "") @PathParam("basketId") basketId: String) =
    Action.async { implicit request =>
      BasketMongoDBService().deleteByBasketId(basketId).map {case basketEntries =>
        render {
          case Accepts.Json() => Ok(Json.toJson(basketEntries))
        }
      }.recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }

    }

}
