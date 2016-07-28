package ch.isbsib.proteomics.mzviz.controllers.uploads

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.uploads.{LoaderMascotData, LoaderMQData}
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

@Api(value = "/uploads", description = "uploads")
object ZipDataController extends CommonController {

  @ApiOperation(nickname = "options",
    value = "empty options method",
    notes = """returns Ok to fulfill the pre-flight OPTIONS request""",
    response = classOf[String],
    httpMethod = "OPTIONS")
  def options(@ApiParam(value = """resultType""") @PathParam("resultType") resultType: String) =
    Action {
      Ok("Okay").withHeaders(
        ACCESS_CONTROL_ALLOW_HEADERS ->  s"$ORIGIN, X-Requested-With, $CONTENT_TYPE, $ACCEPT, $AUTHORIZATION, X-Auth-Token"
      )
    }

  @ApiOperation(nickname = "loadZip",
    value = "Loads a zip file",
    notes = """ source will be a unique descriptor on the source""",
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "zip file", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadZip(@ApiParam(value = """resultType""", defaultValue = "maxquant") @PathParam("resultType") resultType: String) =
    Action.async(parse.temporaryFile) {
      request =>
        val intensityThreshold = 30000

        val entries = if(resultType == "maxquant")
                        LoaderMQData().loadZip(request.body.file.getAbsolutePath, intensityThreshold)
                      else
                        LoaderMascotData().loadZip(request.body.file.getAbsolutePath, intensityThreshold)

        entries.map { n => Ok(("File uploaded"))
        }.recover {
          case e => BadRequest(Json.toJson(e))
        }
    }
}
