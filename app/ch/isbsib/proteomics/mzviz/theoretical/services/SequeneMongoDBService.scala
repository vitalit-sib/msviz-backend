package ch.isbsib.proteomics.mzviz.theoretical.services

import ch.isbsib.proteomics.mzviz.commons.services.MongoDBService
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, RefSpectrum}
import ch.isbsib.proteomics.mzviz.experimental.{ScanNumber, IdRun, MSRun}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonFormats._
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api._
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.bson._
import reactivemongo.core.commands.{LastError, GetLastError, RawCommand, Count}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author Alexandre Masselot
 */
class SequenceMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "sequences"

  val indexes = List(new Index(
    Seq("accessionCode" -> IndexType.Ascending, "source" -> IndexType.Ascending),
    name = Some("ac_source"),
    unique = true))

  /**
   * insert a list of Fasta entries
   * @param entries to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(entries: Seq[FastaEntry]): Future[Int] = {
    //    val enumerator = Enumerator(run.msnSpectra: _*)
    //    msnSpectraCollection.bulkInsert(enumerator)
    ???
  }

  /**
   * remove all entries from the mongodb
   * @param source the datasource
   * @return
   */
  def deleteAllBySource(source: String): Future[Boolean] = ???

  /**
   * retieves all entris for a given source
   * @param source the data source
   * @return
   */
  def findAllEntriesBySource(source: String): Future[Seq[FastaEntry]] = ???

  /**
   *
   * @param accessionCode entry AC
   * @param source data source
   * @return
   */
  def findEntryByAccessionCodeAndSource(accessionCode: String, source: String): Future[FastaEntry] = ???

  /**
   * GEt the list of data sources
   * @return
   */
  def listSources: Future[Seq[String]] = ???

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


object SequenceMongoDBService extends Controller with MongoController {
  val default = new SequenceMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default


}
