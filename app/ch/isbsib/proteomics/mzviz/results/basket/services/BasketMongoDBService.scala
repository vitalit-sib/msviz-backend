package ch.isbsib.proteomics.mzviz.results.basket.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.results.basket.models.BasketEntry
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import ch.isbsib.proteomics.mzviz.results.basket.services.JsonBasketFormats._
import reactivemongo.bson.{BSON, BSONDocument}
import reactivemongo.core.commands.{LastError, Count, RawCommand}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


import scala.concurrent.Future

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class BasketMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "basket"
  val mainKeyName = "searchIds"


  setIndexes(List(
    new Index(
      Seq("searchIds" -> IndexType.Text),
      name = Some("searchIds")
    )
  ))

  /**
   * insert a list of basket entries
   * @param newEntries
   * @return a Future of the number of entries loaded
   */
  def insert(newEntries: Seq[BasketEntry]): Future[Int] = {
    for {
      n <- collection.bulkInsert(Enumerator(newEntries: _*))
    } yield n
  }

  /**
   * list for given searchIds all proteins
   * @param searchIds
   * @return
   */
  def listProteins(searchIds: String): Future[Seq[AccessionCode]] ={

    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "proteinAC", "query" -> BSONDocument("searchIds" -> searchIds)))
    db.command(command)
      .map({
        doc =>
          doc.getAs[List[String]]("values").get
            .map {
              i => AccessionCode(i)
            }
      })
  }

  /**
   * count the number of Spectra
   * @return
   */
  def countBasketEntries: Future[Int] = {
    db.command(Count(collectionName))
  }

  /**
   * delete all entries which use this searchId
   * @return a Future of number of deleted entries
   */
  def deleteBySearchId(searchId: String): Future[Boolean] = {

    val query = Json.obj("$text" -> Json.obj("$search" -> searchId))

    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

}
