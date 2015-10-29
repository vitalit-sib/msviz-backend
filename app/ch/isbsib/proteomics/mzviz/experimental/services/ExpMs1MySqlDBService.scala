package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.experimental.models.Ms1Peak

import play.api.db.slick.Config.driver.simple._
import play.api.mvc._



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

  def mozIndex = index("moz", moz, unique = false)
  def refIndex = index("ref", ref, unique = false)
}


/**
 * accompaniant object
 */

object ExpMs1MySqlDBService extends Controller {
  val ms1Dao = TableQuery[ExpMs1MySqlDBService]

  /**
   * get the default database
   * @return
   */
  def apply() = ms1Dao

}
