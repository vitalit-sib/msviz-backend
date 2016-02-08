package ch.isbsib.proteomics.mzviz.results.basket

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.results.basket.models.{BasketEntryWithSpInfo, BasketEntry}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import play.api.libs.json._
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import JsonBasketFormats._
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import reactivemongo.core.commands.{LastError, Count, RawCommand}
import scala.concurrent.ExecutionContext.Implicits.global
import ch.isbsib.proteomics.mzviz.commons.services.MongoId


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

    val inserted: Seq[Future[Int]] = newEntries.map({ entry =>
      val selector = Json.obj(
        "proteinAC" -> entry.proteinAC.value,
        "peptideSeq" -> entry.peptideSeq,
        "startPos" -> entry.startPos,
        "searchIds" -> entry.searchIds,
        "spectrumId.id" -> entry.spectrumId.id.value)

      val answer = collection.update(selector, entry, upsert = true)

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
            sp.ref.scanNumber, sp.ref.precursor.retentionTime.value/60, sp.ref.precursor.charge.value, sp.ref.precursor.moz.value,
            e.score, e.localizationScore, e.ppmTolerance, e.rtZoom, e.rtSelected, e.xicPeaks)
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

  /**
   * list of searchIds in the basket
   *
   * @return
   */
  def listSearchIds: Future[Seq[String]] ={

    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "searchIds"))
    db.command(command)
      .map({
        doc =>
          doc.getAs[List[String]]("values").get
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
   * @return a Future of boolean
   */
  def deleteBySearchId(searchId: String): Future[Boolean] = {

    val query = Json.obj("$text" -> Json.obj("$search" -> searchId))

    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
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

}


object BasketMongoDBService extends Controller with MongoController {
  val default = new BasketMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default

}
