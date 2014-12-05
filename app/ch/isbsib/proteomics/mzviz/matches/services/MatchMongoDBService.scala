package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.SpectraSource
import ch.isbsib.proteomics.mzviz.commons.services.MongoDBService
import ch.isbsib.proteomics.mzviz.matches.models.PepSpectraMatch
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.core.commands.Count
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by Roman Mylonas on 04/12/14.
 */
class MatchMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "matches"

  val indexes = List(new Index(
    Seq("accessionCode" -> IndexType.Ascending, "source" -> IndexType.Ascending),
    name = Some("ac_source"),
    unique = true))

  /**
   * insert a list of Match entries
   * @param entries to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(entries: Seq[PepSpectraMatch]): Future[Int] = {
    //    val enumerator = Enumerator(run.msnSpectra: _*)
    //    msnSpectraCollection.bulkInsert(enumerator)
    ???
  }

  /**
   * remove all entries from the mongodb
   * @param source the datasource
   * @return
   */
  def deleteAllBySource(source: SpectraSource): Future[Boolean] = ???

  /**
   * retrieves all entries for a given source
   * @param source the data source
   * @return
   */
  def findAllEntriesBySource(source: SpectraSource): Future[Seq[PepSpectraMatch]] = ???

  /**
   *
   * @param accessionCode entry AC
   * @param source data source
   * @return
   */
  def findEntryByProteinMatch(accessionCode: AccessionCode, source: SequenceSource): Future[FastaEntry] = ???

  /**
   * Get the list of data sources
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


object MatchMongoDBService extends Controller with MongoController {
  val default = new MatchMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default

}
