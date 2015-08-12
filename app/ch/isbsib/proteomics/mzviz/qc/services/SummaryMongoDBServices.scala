package ch.isbsib.proteomics.mzviz.qc.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.qc.models.QcSummaryEntry


import ch.isbsib.proteomics.mzviz.qc.services.JsonQCFormats._
import ch.isbsib.proteomics.mzviz.theoretical.SequenceSource
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api._
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.core.commands.{Count, LastError}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by qjolliet on 04/08/15.
 */
class SummaryMongoDBServices(val db: DefaultDB) extends MongoDBService  {
  val collectionName = "summary"
  val mainKeyName = "rawfileInfomation.Date"

  setIndexes(List(
    new Index(Seq("rawfileInfomation.Date"->IndexType.Ascending,"rawfileInfomation.Index"->IndexType.Ascending),name = Some("Date")),
    new Index(
      Seq("rawfileInfomation" -> IndexType.Ascending), name = Some("RawfileInfomation"),unique = true))
  )
  /**
   * insert a list of Summary entries
   * @param   entries to be inserted
   * @return a Future of the number of entries loaded
   */

  def insert(entries: Seq[QcSummaryEntry]):Future[Int] ={
    val enumerator = Enumerator.enumerate(entries)
    collection.bulkInsert(enumerator)
  }
  /**
   * retieves all entries for a given date
   * @param d the date
   * @return
   */
  def findAllByDate(d: String): Future[Seq[QcSummaryEntry]] = {
    val query = Json.obj("rawfileInfomation.Date" -> d)
    collection.find(query).cursor[QcSummaryEntry].collect[List]()
  }
  /**
   * remove all entries from the mongodb
   * @param d the date
   * @return
   */
  def deleteAllByDate(d: String): Future[Boolean] = {
    val query = Json.obj("rawfileInfomation.Date" -> d)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   * count the number of Entries
   * @return
   */
  def countSummary: Future[Int] = {
    db.command(Count(collectionName))
  }
}



object SummaryMongoDBServices extends Controller with MongoController {
  val default = new SummaryMongoDBServices(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default


}