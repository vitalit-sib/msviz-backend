package ch.isbsib.proteomics.mzviz.controllers

import java.io.File

import play.api.mvc.{Action, Controller}
import play.api.{Logger, Play}
import play.api.Play.current

import scala.io.Source

/**
  * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
  *         copyright 2014-2017, SIB Swiss Institute of Bioinformatics
  */
object UtilsController extends Controller{

  def hello = Action{
    Logger.info("hello logger")
    Ok("hello web")
  }

  def version = Action{
    val myFile:File = Play.getFile("conf/version.txt")
    val msVizVersion:String = Source.fromFile(myFile).getLines.mkString
    Ok(msVizVersion)
  }

}
