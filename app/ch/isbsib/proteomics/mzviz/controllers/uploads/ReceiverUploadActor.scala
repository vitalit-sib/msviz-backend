package ch.isbsib.proteomics.mzviz.controllers.uploads

import java.io.File

import akka.actor.{ActorSystem, Props, Actor}

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

class ReceiverUploadActor extends Actor {
  def receive = {
    case "inserted_end" =>{
      val p = new java.io.PrintWriter(new File("wee.txt"))
      p.print("doneee")
      p.close()
    }
  }
}
