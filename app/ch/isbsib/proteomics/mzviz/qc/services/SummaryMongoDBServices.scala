package ch.isbsib.proteomics.mzviz.qc.services

import ch.isbsib.proteomics.mzviz.commons.services.MongoDBService
import ch.isbsib.proteomics.mzviz.qc.models.QcSummaryEntry


import ch.isbsib.proteomics.mzviz.qc.services.JsonQCFormats._
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api._
import reactivemongo.api.indexes.{Index, IndexType}

import scala.concurrent.ExecutionContext.Implicits.global
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

  def insert(entries: Seq[QcSummaryEntry]) = {
    val enumerator = Enumerator.enumerate(entries)
    collection.bulkInsert(enumerator)
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