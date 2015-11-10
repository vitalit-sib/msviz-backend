package ch.isbsib.proteomics.mzviz.controllers.results

import ch.isbsib.proteomics.mzviz.controllers.matches.SearchController._
import ch.isbsib.proteomics.mzviz.results.basket.BasketMongoDBService
import ch.isbsib.proteomics.mzviz.results.basket.models.BasketEntry
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.libs.concurrent.Execution.Implicits._
import ch.isbsib.proteomics.mzviz.results.basket.JsonBasketFormats._

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/basket", description = "basket")
object BasketController {

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

}
