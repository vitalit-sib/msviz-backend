package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{ProteinIdent, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json, JsArray}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.core.commands.{Count, LastError}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ProteinMatchMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "proteinMatches"
  val mainKeyName = "searchId"

  setIndexes(List(
    new Index(
      Seq("searchId" -> IndexType.Ascending),
      name = Some("searchId_source"),
      unique = false),
    new Index(
      Seq("mainProt.proteinAC" -> IndexType.Ascending),
      name = Some("proteinRef"),
      unique = false)
  ))

  /**
   * insert a list of Match entries
   * @param matches to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(matches: Seq[ProteinIdent]): Future[Int] = {
    val searchIds = matches.map(_.searchId.value).toSet
    for {
      c <- checkIfAnyKeyExist(searchIds)
      n <- collection.bulkInsert(Enumerator(matches: _*))
    } yield n
  }

  /**
   * remove all entries from the mongodb
   * @param searchId the search id
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
   * remove all entries from the mongodb
   * @param searchIds mutliple search ids
   * @return
   */
  def deleteAllBySearchIds(searchIds: Set[SearchId]): Future[Boolean] = {
    val query = Json.obj("searchId" -> Json.obj("$in" -> searchIds.toList))
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
  def findAllProteinsBySearchId(searchId: SearchId): Future[Seq[ProteinIdent]] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.find(query).cursor[ProteinIdent].collect[List]()
  }

  /**
   * retrieves all entries for a list of sources
   * @param searchIds list of search ids
   * @return
   */
  def findAllProteinsBySearchIds(searchIds: Set[SearchId]): Future[Seq[ProteinIdent]] = {
    val query = Json.obj("searchId" -> Json.obj("$in" -> searchIds.toList))
    collection.find(query).cursor[ProteinIdent].collect[List]()
  }


  /**
   * count the number of Entries
   * @return
   */
  def countEntries: Future[Int] = {
    db.command(Count(collectionName))
  }

}


object ProteinMatchMongoDBService extends Controller with MongoController {
  val default = new ProteinMatchMongoDBService(db)

  /**
   * get the default db/collection
   * @return
   */
  def apply() = default

}