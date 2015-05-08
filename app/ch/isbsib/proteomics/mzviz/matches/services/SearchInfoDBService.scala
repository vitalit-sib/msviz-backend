package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoDBService, MongoNotFoundException}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.SearchInfo
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._

import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.{LastError, RawCommand}

import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SearchInfoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "searchInfo"
  val mainKeyName = "searchId"

  setIndexes(List(
    new Index(
      Seq("searchId" -> IndexType.Ascending),
      name = Some("searchId"),
      unique = true)
  ))

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
  def delete(searchId: SearchId): Future[Boolean] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   * retrieves searchInfo for a given searchId
   * @param searchId the search id
   * @return
   */
  def get(searchId: SearchId): Future[Option[SearchInfo]] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.find(query).cursor[SearchInfo].headOption
  }


  /**
   * retrieves a list of SearchId's
   * @return
   */
  def list: Future[List[SearchInfo]] = {
    collection.find(Json.obj()).cursor[SearchInfo].collect[List]()
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

  /**
   * check if a results has already been loaded for the given searchId
   * @param searchId search key
   */
  def isSearchIdExist(searchId: SearchId): Future[Boolean] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.find(query)
      .cursor[JsObject]
      .headOption
      .map(_.isDefined)
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


