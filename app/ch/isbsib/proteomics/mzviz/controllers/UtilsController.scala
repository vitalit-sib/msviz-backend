package ch.isbsib.proteomics.mzviz.controllers

import play.api.mvc.{Action, Controller}

/**
 * Created by amasselo on 4/7/15.
 */
object UtilsController extends Controller{

  def hello = Action{
    Ok("world")
  }
}
