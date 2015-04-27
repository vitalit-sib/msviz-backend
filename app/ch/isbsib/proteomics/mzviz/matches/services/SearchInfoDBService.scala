package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.controllers.routes
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{SearchInfo, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.theoretical.SequenceSource
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import play.api.libs.concurrent.Execution.Implicits._
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.{RawCommand, LastError}
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._

import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SearchInfoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "searchInfo"
  val mainKeyName = "searchId"


  /**
   * insert a list of SearchInfo entries
   * @param entries to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(entries: Iterator[SearchInfo]): Future[Int] = {
    val enumerator = Enumerator.enumerate(entries)
    collection.bulkInsert(enumerator)
  }


  /**
   * remove all entries from the mongodb
   * @param searchId the seach id
   * @return
   */
  def deleteAllBySearchId(searchId: SearchId): Future[Boolean] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   * retrieves all entries for a given searchId
   * @param searchId the search id
   * @return
   */
  def findAllSearchInfoBySearchId(searchId: SearchId): Future[Seq[SearchInfo]] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.find(query).cursor[SearchInfo].collect[List]()
  }


  /**
   * retrieves a list of SearchId's
   * @return
   */
  def listSearchIds: Future[Seq[SearchId]] = {
    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "searchId"))
    db.command(command)
      .map({
      doc =>
        doc.getAs[List[String]]("values").get
          .map {
          i => SearchId(i)
        }
    })
  }
}

object SearchInfoDBService extends Controller with MongoController {
  val default = new SearchInfoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default


}


