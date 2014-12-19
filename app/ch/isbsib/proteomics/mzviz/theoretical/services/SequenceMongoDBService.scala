package ch.isbsib.proteomics.mzviz.theoretical.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api._
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.{BSONString, BSONDocument}
import reactivemongo.core.commands.{LastError, Remove, RawCommand, Count}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
class SequenceMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "sequences"

  setIndexes(List(
    new Index(
      Seq("ac" -> IndexType.Ascending, "source" -> IndexType.Ascending),
      name = Some("ac_source"),
      unique = true)
  ))

  /**
   * insert a list of Fasta entries
   * @param entries to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(entries: Iterator[FastaEntry]): Future[Int] = {
    val enumerator = Enumerator.enumerate(entries)
    collection.bulkInsert(enumerator)
  }

  /**
   * remove all entries from the mongodb
   * @param source the datasource
   * @return
   */
  def deleteAllBySource(source: SequenceSource): Future[Boolean] = {
    val query = Json.obj("proteinRef.source" -> source.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   * retieves all entries for a given source
   * @param source the data source
   * @return
   */
  def findAllEntriesBySource(source: SequenceSource): Future[Seq[FastaEntry]] = {
    val query = Json.obj("proteinRef.source" -> source.value)
    collection.find(query).cursor[FastaEntry].collect[List]()
  }

  /**
   *
   * @param accessionCode entry AC
   * @param source data source
   * @return
   */
  def findEntryByAccessionCodeAndSource(accessionCode: AccessionCode, source: SequenceSource): Future[FastaEntry] = {
    val query = Json.obj("proteinRef.AC" -> accessionCode.value, "proteinRef.source" -> source.value)
    collection.find(query).cursor[FastaEntry].headOption map {
      case Some(fe: FastaEntry) => fe
      case None => throw new MongoNotFoundException(s"${source.value}/$accessionCode")
    }
  }

  /**
   * GEt the list of data sources
   * @return
   */
  def listSources: Future[Seq[SequenceSource]] = {
    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "proteinRef.source"))
    db.command(command)
      .map({
      doc =>
        doc.getAs[List[String]]("values").get
          .map {
          i => SequenceSource(i)
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
   * count the number of data sequence for a given source
   * @return
   */
  def countSequencesBySource (source: SequenceSource): Future[Int] = {
    db.command(Count(collectionName, Some(BSONDocument("proteinRef.source" -> source.value))))
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
