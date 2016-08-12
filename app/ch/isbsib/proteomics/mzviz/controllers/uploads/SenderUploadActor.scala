package ch.isbsib.proteomics.mzviz.controllers.uploads


import akka.actor.{ActorRef, Actor}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.uploads.{LoaderMascotData, LoaderMQData}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


class SenderUploadActor(receiverUploadActor: ActorRef) extends Actor {
  def receive = {

    case ZipUploadData(path, intensityThreshold, resultType) => {
      // we don't send anything to the sender, since he's not listening
      //sender ! ("started")

      val entries: Future[Seq[SearchId]] =  if (resultType == "maxquant")
                                                LoaderMQData().loadZip(path, intensityThreshold)
                                              else {
                                                LoaderMascotData().loadZip(path, intensityThreshold)
                                              }

      // send the result (a Future) to the receiver
      receiverUploadActor ! entries

      // stop the actor
      context.stop(self)

    }
  }
}

case class ZipUploadData(path: String, intensityThreshold: Double, resultType: String)
