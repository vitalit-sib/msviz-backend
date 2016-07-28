package ch.isbsib.proteomics.mzviz.controllers.uploads


import java.io.File

import akka.actor.{ActorSystem, Props, Actor}
import ch.isbsib.proteomics.mzviz.controllers.uploads.ZipDataController._
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.uploads.{LoaderMascotData, LoaderMQData}
import play.api.libs.json.Json
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

      val entries: Future[Seq[SearchId]] = if(resultType == "maxquant")
        LoaderMQData().loadZip(path, intensityThreshold)
      else
        LoaderMascotData().loadZip(path, intensityThreshold)

      entries.map { n =>  val receiver = acsy.actorOf(Props(new ReceiverUploadActor(n,"done")), "upload")
        receiver ! "inserted_end"
      }
    }
  }
}
