package ch.isbsib.proteomics.mzviz.controllers.uploads

import java.io.File

import akka.actor.{ActorSystem, Props, Actor}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.services.SearchInfoDBService

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


/*
 class ReceiverUploadActor (m:String) extends Actor{
  def receive() {
    m match {
      case "inserted" => {
        println(s"inserted")

      }
    }
  }
}
*/

class ReceiverUploadActor(searchIds: Seq[SearchId],status:String) extends Actor {
  def receive = {
    case "inserted_end" =>{
      val p = new java.io.PrintWriter(new File("wee.txt"))
      p.print("doneee")
      p.close()

      searchIds.map {searchId =>SearchInfoDBService().updateStatus(searchId, status)}


    }
  }
}
