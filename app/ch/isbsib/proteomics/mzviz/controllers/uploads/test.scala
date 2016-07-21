package ch.isbsib.proteomics.mzviz.controllers.uploads


import akka.actor.{ActorSystem, Props, Actor}
import akka.actor.Actor._
import ch.isbsib.proteomics.mzviz.controllers.CommonController
import com.wordnik.swagger.annotations.{ApiImplicitParam, ApiImplicitParams, ApiOperation, Api}
import play.api.mvc._


/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

@Api(value = "/test", description = "test")
object test extends CommonController {


  @ApiOperation(nickname = "test",
    value = "test",
    notes = """test""",
    response = classOf[String],
    httpMethod = "POST")
  def we =
    Action{
      val acsy = ActorSystem("WSU-CEG-7370-Actors")
      val grtr = acsy.actorOf(Props[UploadActor], "greeter")
      grtr ! "start"
      Ok("Ok")
    }






}