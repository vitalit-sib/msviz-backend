package ch.isbsib.proteomics.mzviz.controllers.uploads


import javax.ws.rs.PathParam

import akka.actor.{ActorSystem, Props, Actor}
import akka.actor.Actor._
import ch.isbsib.proteomics.mzviz.controllers.CommonController
import com.wordnik.swagger.annotations.{ApiImplicitParam, ApiImplicitParams, ApiOperation, Api}
import play.api.mvc._



import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.uploads.{LoaderMQData, LoaderMascotData}
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._

import scala.concurrent.Future


/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

@Api(value = "/test", description = "test")
object ActorsUpload extends CommonController {
  @ApiOperation(nickname = "test",
    value = "test",
    notes = """test""",
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "file or folder", required = true, dataType = "text/plain", paramType = "body")
  ))
  def upload (@ApiParam(value = """resultType""", defaultValue = "maxquant") @PathParam("resultType") resultType: String,
          @ApiParam(name = "intensityTh", value = """intensity Threshold""", defaultValue = "1", required=false) @PathParam("intensityTh") intensityTh:Option[Double]) =
    Action(parse.temporaryFile){

      request =>
        val actorSystem = ActorSystem()

        val receiverActor = actorSystem.actorOf(Props(new ReceiverUploadActor()), "receive-upload-answer")
        val uploadActor = actorSystem.actorOf(Props(new SenderUploadActor(receiverActor)), "start-upload")

        uploadActor ! new ZipUploadData(request.body.file, intensityTh.getOrElse(1.0), resultType)

        Ok("insertion was started")
    }






}