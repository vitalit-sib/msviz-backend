package ch.isbsib.proteomics.mzviz.controllers.uploads


import java.io.File

import akka.actor.{ActorSystem, Props, Actor}
import ch.isbsib.proteomics.mzviz.controllers.uploads.ZipDataController._
import ch.isbsib.proteomics.mzviz.uploads.{LoaderMascotData, LoaderMQData}
import play.api.libs.json.Json
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


class SenderUploadActor(path: String, intensityThreshold: Double, resultType: String) extends Actor {
  def receive = {

    case "start" => {
      println("actor uploading")
      sender ! (s"starting")

      val acsy = ActorSystem("WSU-CEG-7370-Actors")

      ///TO DO insert

      val entries = if(resultType == "maxquant")
        LoaderMQData().loadZip(path, intensityThreshold)
      else
        LoaderMascotData().loadZip(path, intensityThreshold)

      entries.map { n =>  val receiver = acsy.actorOf(Props[ReceiverUploadActor], "upload")
        receiver ! "inserted_end"
      }
    }
  }
}
