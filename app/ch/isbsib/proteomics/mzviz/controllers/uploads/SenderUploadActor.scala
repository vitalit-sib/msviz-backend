package ch.isbsib.proteomics.mzviz.controllers.uploads


import java.io.File

import akka.actor.{ActorRef, Actor}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.uploads.{LoaderMascotData, LoaderMQData}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import akka.event.Logging

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


class SenderUploadActor(receiverUploadActor: ActorRef) extends Actor {
  val log = Logging(context.system, this)

  def receive = {

    case ZipUploadData(path, intensityThreshold, resultType) => {

      // we don't send anything to the sender, since he's not listening
      //sender ! ("started")

      try {
        val entries: Future[Seq[SearchId]] = if (resultType == "maxquant")
          LoaderMQData().loadZip(path, intensityThreshold)
        else {
          LoaderMascotData().loadZip(path, intensityThreshold)
        }

        // send the result (a Future) to the receiver
        receiverUploadActor ! entries

      } catch {
        case e: Exception => log.error(e, "Catched an error in SenderUploadActor")
      } finally {
        // stop the actor
        context.stop(self)
      }

    }
  }
}

case class ZipUploadData(zipFile: File, intensityThreshold: Double, resultType: String)
