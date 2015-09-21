package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.experimental.models.{Ms1Entry, Ms1Peak}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMs1MongoDBService._
import play.api.data.Forms.default
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.{Failure, Success}
import play.api._
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import play.api.Play.current
import play.api.libs.json.Json
import play.api.libs.json.Json._

import scala.slick.jdbc.meta.MTable


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1MySqlDBService(tag: Tag) extends Table[Ms1Peak](tag, "MS") {

  def ref = column[String]("ref", O.NotNull)
  def int = column[Double]("int", O.NotNull)
  def rt = column[Double]("rt", O.NotNull)
  def moz = column[Double]("moz", O.NotNull)

  def * = (ref, rt, moz, int) <> (Ms1Peak.tupled, Ms1Peak.unapply _)
}


/**
 * accompaniant object
 */

object ExpMs1MySqlDBService extends Controller {
  val ms1Dao = TableQuery[ExpMs1MySqlDBService]

//  def getXic(name: String) = {
//    ms1Dao.filter(ms => ms.ref === name && ms.rt < 34.0)
//  }
//
//  def insertMs1Peak(ref: String, rt: Double, moz: Double) = {
//    val onePeak = new Ms1Peak(ref, rt, moz)
//    ms1Dao.insert(onePeak)
//  }

  /**
   * get the default database
   * @return
   */
  def apply() = ms1Dao
}
