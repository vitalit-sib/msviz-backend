package ch.isbsib.proteomics.mzviz.controllers.uploads

import java.util.Calendar

import akka.actor.Actor
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.SubmissionStatus
import ch.isbsib.proteomics.mzviz.matches.services.SearchInfoDBService
import play.api.Logger
import reactivemongo.api.DefaultDB
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import akka.event.Logging

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


class ReceiverUploadActor() extends Actor {
  val log = Logging(context.system, this)

  def receive = {

    // got the result from the sender
    case res: Future[Seq[SearchId]] => {
      res.onComplete({
        case Success(ids) => {
          log.info("Ok loaded ids " + ids)
          val statusUpdated:Seq[Future[Boolean]] = ids.map({ id =>
            val status = new SubmissionStatus(code="done", message = "All data was successfully loaded.")
            SearchInfoDBService().updateStatus(id, status)
          })

          Future.sequence(statusUpdated).map(a => a.reduceLeft(_ & _)).onComplete({
            case Success(ids) => log.info("Status was set to done")
            case Failure(e) => log.error(e, "Could not set status")
          })
        }
        case Failure(e) => {
          log.error(e, "Got an error from the SenderUploadActor: " + e.getMessage)
        }
      })

      // stop the actor
      context.stop(self)
    }
  }
}
