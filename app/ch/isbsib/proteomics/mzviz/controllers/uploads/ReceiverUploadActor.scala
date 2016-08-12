package ch.isbsib.proteomics.mzviz.controllers.uploads

import akka.actor.Actor
import ch.isbsib.proteomics.mzviz.matches.SearchId
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


class ReceiverUploadActor() extends Actor {
  def receive = {

    // got the result from the sender
    case res: Future[Seq[SearchId]] => {
      res.onComplete({
        case Success(ids) => println("Ok inserted ids " + ids)
        case Failure(e) => println("Got exception: " + e.getMessage)
      })

      // stop the actor
      context.stop(self)
    }
  }
}
