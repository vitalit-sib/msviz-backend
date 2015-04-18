package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoDBService, MongoNotFoundException}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{PepSpectraMatch, ProteinRef, SearchInfo}
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.Logger
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.{BSONArray, BSONDocument}
import reactivemongo.core.commands._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
 * Store/retrieves SearchInfo into mongodb
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SearchInfoMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "searchInfos"
  val mainKeyName = "searchId"

  setIndexes(List(
    new Index(
      Seq("searchId" -> IndexType.Ascending),
      name = Some("searchId_source"),
      unique = false)
  ))

  /**
   * insert a searchInfo
   * @param searchInfo to be inserted
   * @return a Future LastError
   */
  def insert(searchInfo: SearchInfo): Future[Boolean] = {
      collection.insert(searchInfo).map{
        case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
        case _ => true
      }
  }

  /**
   * remove all entries from the mongodb
   * @param searchId the searchId
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
   * remove all entries from the mongodb
   * @param searchIds mutliple search ids
   * @return
   */
  def deleteAll(searchIds: List[SearchId]): Future[Boolean] = {
    val query = Json.obj("searchId" -> Json.obj("$in" -> searchIds))
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   * retrieves all entries for a given source
   * @param searchId the search id
   * @return
   */
  def get(searchId: SearchId): Future[SearchInfo] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.find(query).cursor[SearchInfo].headOption map {
      case Some(fe: SearchInfo) => fe
      case None => throw new MongoNotFoundException(s"searchId")
    }

  }

  /**
   * check if a results has already been loaded for the given searchId
   * @param searchId search key
   */
  def exists(searchId: SearchId): Future[Boolean] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.find(query)
      .cursor[JsObject]
      .headOption
      .map(_.isDefined)
  }


  /**
   * list all searchInfo
   *
   * @return list of SearchId
   */

  def list: Future[Seq[SearchInfo]] = {
    collection.find().cursor[SearchInfo].collect[List]()
  }


}

object SearchInfoMongoDBService extends Controller with MongoController {
  val default = new SearchInfoMongoDBService(db)

  /**
   * get the default db/collection
   * @return
   */
  def apply() = default

}
