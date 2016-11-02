package ch.isbsib.proteomics.mzviz.results.basket

import java.util.{Date, Calendar}

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.results.basket.models.{BasketEntryWithSpInfo, BasketEntry}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import play.api.libs.json._
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import JsonBasketFormats._
import reactivemongo.bson.{BSONValue, BSONObjectID, BSONArray, BSONDocument}
import reactivemongo.core.commands.{LastError, Count, RawCommand}
import scala.concurrent.ExecutionContext.Implicits.global


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
  def insertOrUpdate(newEntries: Seq[BasketEntry]): Future[Int] = {

    // get current time
    val currentTime:Date = Calendar.getInstance().getTime()

    val inserted: Seq[Future[Int]] = newEntries.map({ entry =>

      val selector = Json.obj(
        "proteinAC" -> entry.proteinAC.value,
        "peptideSeq" -> entry.peptideSeq,
        "startPos" -> entry.startPos,
        "searchIds" -> entry.searchIds,
        "spectrumId.id" -> entry.spectrumId.id.value)

      // copy the basket entry and add the time fields
      val newEntry = entry.copy(creationDate=Some(currentTime))

      val answer = collection.update(selector, newEntry, upsert = true)

      answer.map({
        case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
        case _ => 1
      })
    })

    // sum of all inserted or updated
    Future.sequence(inserted).map(res => res.sum)

  }

  /**
   * find all entries for a certain protein and searchId
   * @param searchIds
   * @param proteinAC
   * @return
   */
  def findByProtein(searchIds: String, proteinAC: AccessionCode): Future[Seq[BasketEntry]] ={
    val query = Json.obj("searchIds" -> searchIds, "proteinAC" -> proteinAC.value)

    collection.find(query)
      .cursor[JsObject]
      .collect[List]()
      .map(lo => lo.map({ o =>
        Json.fromJson[BasketEntry](o).asOpt.get
      }))
  }

  /**
   * find all entries of a searchId
   * @param searchIds
   * @return
   */
  def findBySearchId(searchIds: String): Future[Seq[BasketEntry]] ={
    val query = Json.obj("searchIds" -> searchIds)

    collection.find(query)
      .cursor[JsObject]
      .collect[List]()
      .map(lo => lo.map({ o =>
        Json.fromJson[BasketEntry](o).asOpt.get
      }))
  }

  /**
   * find all entries of a searchId with spectra info
   * @param runId
   * @return
   */

  def findBySearchIdWithSpInfo(runId: String): Future[Seq[BasketEntryWithSpInfo]] = {
    val basketEntries = findBySearchId(runId)

    basketEntries.flatMap({ l =>
      val flf = l.map({ e =>
        val spFut = ExpMongoDBService().findSpectrumBySpId(e.spectrumId)

        spFut.map({ sp =>
          new BasketEntryWithSpInfo(e._id, e.proteinAC, e.peptideSeq, e.startPos, e.endPos, e.searchIds, e.spectrumId,
            sp.ref.scanNumber.get, sp.ref.precursor.retentionTime.value/60, sp.ref.precursor.charge.value, sp.ref.precursor.moz.value,
            e.score, e.localizationScore, e.ppmTolerance, e.rtZoom, e.rtSelected, e.xicPeaks, e.creationDate, e.prevAA, e.nextAA)
        })

      })

      Future.sequence(flf)

    })

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

  import play.modules.reactivemongo.json.BSONFormats._

  /**
   * list of searchIds in the basket
   *
   * @return
   */
  def listSearchIds: Future[Seq[JsValue]] ={

    val command = RawCommand(BSONDocument("aggregate" -> collectionName,
      "pipeline" -> BSONArray(
        BSONDocument("$group" -> BSONDocument("_id" -> "$searchIds",
          "lastModif" -> BSONDocument("$max" -> "$creationDate"),
          "firstModif" -> BSONDocument("$min" -> "$creationDate")
        ))
    )))
    db.command(command).map({
      doc =>
        doc.getAs[List[BSONDocument]]("result").get.map({
          elDoc: BSONDocument =>
          Json.toJson(elDoc)
        })
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
   * delete entry by given MongoDB $oid (MongId)
   * @return a Future of boolean
   */
  def deleteByMongoId(id: String): Future[Boolean] = {
    val selector = BSONDocument("_id" -> BSONObjectID(id))

    bsonCollection.remove(selector).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

    /**
     * delete all entries with the given Basket Id (which is a concatenation of SearchIds seperated by commas)
     * @return a Future of boolean
     */
    def deleteByBasketId(basketId: String): Future[Boolean] = {

      val selector = BSONDocument("searchIds" -> basketId)

      bsonCollection.remove(selector).map {
        case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
        case _ => true
      }
    }

  /**
   * delete all entries which contain the given SearchId
   * @param searchIds Set of SearchIds
   * @return
   */
  def deleteBySearchId(searchIds: Set[SearchId]): Future[Int] = {

    val resList = Future.sequence(searchIds.map({ searchId =>

      val selector = BSONDocument("xicPeaks.searchId" -> searchId.value)

      bsonCollection.remove(selector).map {
        case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
        case _ => true
      }

    }))

    // get number of trues (nr of deleted searchIds)
    resList.map(res => res.foldLeft(0)((a:Int,b:Boolean) => if(b) a+1 else a))
  }

}


object BasketMongoDBService extends Controller with MongoController {
  val default = new BasketMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default

}
