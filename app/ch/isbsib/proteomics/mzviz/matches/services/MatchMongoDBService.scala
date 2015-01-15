package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumId, SpectrumRef}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{ProteinRef, ProteinMatch, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.BSONFormats
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.bson.{BSONDocument, BSONArray}
import reactivemongo.core.commands._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.modules.reactivemongo.json.BSONFormats._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, Swiss Institute of Bioinformatics
 */
class MatchMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "psm"

  setIndexes(List(new Index(
    Seq("spId" -> IndexType.Ascending, "runId" -> IndexType.Ascending),
    name = Some("id_source"),
    unique = false)))

  /**
   * insert a list of Match entries
   * @param matches to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(matches: Seq[PepSpectraMatch]): Future[Int] = {
    val enumerator = Enumerator(matches: _*)
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
   * retrieves all entries for a given source
   * @param searchId the search id
   * @return
   */
  def findAllSpectrumIdBySearchId(searchId: SearchId): Future[Seq[SpectrumId]] = {
    val query = Json.obj("searchId" -> searchId.value)
    val projection = Json.obj("spectrumId" -> 1, "_id" -> 0)
    collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map(l=>l.map(x=>(x \ "spectrumId").as[SpectrumId]))
  }

  /**
   * retrieves all entries for a given source
   * @param searchId the search id
   * @return
   */
  def findAllPSMBySearchId(searchId: SearchId): Future[Seq[PepSpectraMatch]] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.find(query).cursor[PepSpectraMatch].collect[List]()
  }


  /**
   *
  db.psm.aggregate([
      {$match:{'searchId':'test-a', 'proteinList.proteinRef.AC':'GNAO_HUMAN', 'proteinList.proteinRef.source':'TODO'}},
      {$unwind:'$proteinList'},
      {$match:{'proteinList.proteinRef.AC':'GNAO_HUMAN', 'proteinList.proteinRef.source':'TODO'}},
      {$project:{proteinPosition:'$proteinList', runId:1, spId:1, pep:1, matchInfo:1, _id:0}}
    ]).pretty()
   * @param searchId search to filter on
   * @param source data source
   * @param accessionCode entry AC
   * @return
   */
  def findPSMByProtein(searchId: SearchId, source: SequenceSource, accessionCode: AccessionCode): Future[JsArray] = {
    val command = RawCommand(BSONDocument(
      "aggregate" -> collectionName,
      "pipeline" -> BSONArray(
        BSONDocument("$match" ->
          BSONDocument(
            "searchId" -> searchId.value,
            "proteinList.proteinRef.source" -> source.value,
            "proteinList.proteinRef.AC" -> accessionCode.value)
        ),
        BSONDocument("$unwind" -> "$proteinList"),
        BSONDocument("$match" ->
          BSONDocument(
            "proteinList.proteinRef.source" -> source.value,
            "proteinList.proteinRef.AC" -> accessionCode.value)
        ),
        BSONDocument("$project" ->
          BSONDocument(
            "proteinPosition" -> "$proteinList",
            "spectrumId" -> 1,
            "pep" -> 1,
            "matchInfo" -> 1,
            "searchId" -> 1,
            "_id" -> 0))
      )
    ))
        db.command(command).map({
          doc =>
            JsArray(doc.getAs[List[BSONDocument]]("result").get.map(o => toJSON(o).asInstanceOf[JsObject]))//=>Json.toJson(o))
        })
  }


  /**
   * retrieves a list of SearchId's
   *
   * @return list of SearchId
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
   * retrieves a list of Proteins from one source
   *
   * @param searchId search identifier
   * @return list of Proteins
   */

  def listProteinRefsBySearchId(searchId: SearchId): Future[Seq[ProteinRef]] = {
    val command = RawCommand(BSONDocument(
      "aggregate" -> collectionName,
      "pipeline" -> BSONArray(
        BSONDocument("$match" -> BSONDocument("searchId" -> searchId.value)),
        BSONDocument("$project" -> BSONDocument("proteinList.proteinRef.AC" -> 1, "proteinList.proteinRef.source" -> 1, "searchId" -> 1, "_id" -> 0)),
        BSONDocument("$unwind" -> "$proteinList"),
        BSONDocument("$project" -> BSONDocument("AC" -> "$proteinList.proteinRef.AC", "source" -> "$proteinList.proteinRef.source"))
      )
    ))

    db.command(command).map({
      doc =>
        doc.getAs[List[BSONDocument]]("result").get.map({
          elDoc: BSONDocument =>
            ProteinRef(AccessionCode(elDoc.getAs[String]("AC").get),
              Some(SequenceSource(elDoc.getAs[String]("source").get)))
        }).distinct
    })

  }

  /**
   * Get the list of data sources
   * @return
   */
  def listRunIds: Future[Seq[RunId]] = {
    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "spectrumId.runId"))
    db.command(command)
      .map({
      doc =>
        doc.getAs[List[String]]("values").get
          .map {
          i => RunId(i)
        }
    })
  }

  /**
   * count the number of Entries
   * @return
   */
  def countEntries: Future[Int] = {
    db.command(Count(collectionName))
  }

  /**
   * count the number of data sources
   * @return
   */
  def countRunIds: Future[Int] = {
    listRunIds.map(_.size)
  }

  /**
   * a maps with various counts
   * @return
   */
  def stats: Future[Map[String, Int]] = {
    for {
      nSources <- countRunIds
      nEntries <- countEntries
    } yield {
      Map("sources" -> nSources, "entries" -> nEntries)

    }
  }

}


object MatchMongoDBService extends Controller with MongoController {
  val default = new MatchMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default

}
