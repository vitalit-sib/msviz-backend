package ch.isbsib.proteomics.mzviz.commons.services

import play.api.Logger
import play.api.libs.json.{JsObject, Json}
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.indexes.Index

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
trait MongoDBService {
  val db: DefaultDB
  val collectionName: String

  val mainKeyName: String

  def collection: JSONCollection = db.collection[JSONCollection](collectionName)

  def bsonCollection: BSONCollection = db.collection(collectionName)

  /**
   * Ensure we have the correct indexes
   */
  def setIndexes(indexes: List[Index]): Unit = {
    Logger.info(s"building mongo indexes for $collectionName")

    for {
      idx <- indexes
    } {
      collection.indexesManager.ensure(idx).map {
        b =>
          if (b)
            Logger.info( s"""index [${idx.name.getOrElse("?")}] was created""")
          else
            Logger.info( s"""index [${idx.name.getOrElse("?")}] already exists""")
      }
        .recover {
        case e => Logger.error(s"cannot create index on $collectionName: ${e.getMessage}")
      }

    }
  }

  /**
   * check if any entry exist with the main key
   * @param keyVal a value corresponding to mainKeyName
   * @return
   */
  def isMainKeyExist(keyVal: String): Future[Boolean] = {
    val query = Json.obj(mainKeyName -> keyVal)
    collection.find(query)
      .cursor[JsObject]
      .headOption
      .map(_.isDefined)
  }

  /**
   * if some main key from the the list keys already exists in the collection, will return a Failure
   * @param keys a key list - key shall be projected
   * @return
   */
  def checkIfAnyKeyExist(keys: Set[String]): Future[Unit] = {
    Future.sequence(keys.map(isMainKeyExist))
      .map({ listExist =>
      if (listExist.count(e=>e) != 0) {
        throw new MongoDuplicateKeyException(s"$mainKeyName already exists in $keys")
      }
      Future.successful[Any]()
    })
  }
}

case class MongoNotFoundException(message: String) extends Exception(message)

case class MongoInsertException(message: String) extends Exception(message)

case class MongoDuplicateKeyException(message: String) extends Exception(message)