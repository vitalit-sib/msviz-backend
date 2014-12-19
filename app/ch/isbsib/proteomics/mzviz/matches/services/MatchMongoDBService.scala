package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.SpectraSource
import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.matches.models.{ProteinMatch, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api._
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.{LastError, RawCommand, Count}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
class MatchMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "psm"

  setIndexes(List(new Index(
    Seq("spId" -> IndexType.Ascending, "spSource" -> IndexType.Ascending),
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
   * @param source the spectra source
   * @return
   */
  def deleteAllBySource(source: SpectraSource): Future[Boolean] = {
    val query = Json.obj("spSource" -> source.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   * retrieves all entries for a given source
   * @param source the data source
   * @return
   */
  def findAllEntriesBySource(source: SpectraSource): Future[Seq[JsObject]] = {
    val query = Json.obj("spSource" -> source.value)
    val projection = Json.obj("spId" -> 1, "_id" -> 1)
    collection.find(query, projection).cursor[JsObject].collect[List]()
  }

  /**
   * retrieves all entries for a given source
   * @param source the data source
   * @return
   */
  def findAllPSMByRunId(source: SpectraSource): Future[Seq[PepSpectraMatch]] = {
    val query = Json.obj("spSource" -> source.value)
    collection.find(query).cursor[PepSpectraMatch].collect[List]()
  }


  /**
   *
   * @param accessionCode entry AC
   * @param source data source
   * @return
   */
  def findEntriesByProteinMatch(accessionCode: AccessionCode, source: SequenceSource): Future[Seq[PepSpectraMatch]] = ???


  /**
   * retrieves a list of ProteinMatches from one source
   *
   * @param source data source
   * @return list of ProteinMatches
   */

  def listProteinMatches(source: SequenceSource): Future[Seq[ProteinMatch]] = ???


  /**
   * Get the list of data sources
   * @return
   */
  def listSources: Future[Seq[SpectraSource]] = {
    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "spSource"))
    db.command(command)
      .map({
      doc =>
        doc.getAs[List[String]]("values").get
          .map {
          i => SpectraSource(i)
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
  def countSources: Future[Int] = {
    listSources.map(_.size)
  }

  /**
   * a maps with various counts
   * @return
   */
  def stats: Future[Map[String, Int]] = {
    for {
      nSources <- countSources
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
