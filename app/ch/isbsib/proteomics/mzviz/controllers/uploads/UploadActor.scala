package ch.isbsib.proteomics.mzviz.controllers.uploads

import java.io.File

import akka.actor.Actor

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class UploadActor extends Actor {
  def receive = {
    case "start" =>{
      sender ! (s"starting")


      for(i<- 1 to 60){
        val p = new java.io.PrintWriter(new File("wee.txt"))
        Thread.sleep(1000)
        p.print(i)
        p.close()
      }

    }
  }
}
