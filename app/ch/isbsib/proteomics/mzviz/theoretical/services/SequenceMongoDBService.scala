package ch.isbsib.proteomics.mzviz.theoretical.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.matches.models.ProteinRef
import ch.isbsib.proteomics.mzviz.theoretical.models.{SequenceSourceStats, FastaEntry}
import ch.isbsib.proteomics.mzviz.theoretical.services.JsonTheoFormats._
import ch.isbsib.proteomics.mzviz.theoretical.{ProteinIdentifier, AccessionCode, SequenceSource}
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api._
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.{BSONArray, BSONString, BSONDocument}
import reactivemongo.core.commands.{LastError, Remove, RawCommand, Count}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SequenceMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "sequences"
  val mainKeyName="proteinRef.source"


  setIndexes(List(
    new Index(
      Seq("proteinRef.identifiers" -> IndexType.Ascending, "proteinRef.source" -> IndexType.Ascending),
      name = Some("proteinRef"),
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
      case None => throw new MongoNotFoundException(s"$source/$accessionCode")
    }
  }

  /**
   *
   * @param id any entry indetifier
   * @param source data source
   * @return
   */
  def findEntryByIdentifierAndSource(id: ProteinIdentifier, source: SequenceSource): Future[FastaEntry] = {
    val query = Json.obj("proteinRef.identifiers" -> id.value, "proteinRef.source" -> source.value)
    println(query)
    collection.find(query).cursor[FastaEntry].headOption map {
      case Some(fe: FastaEntry) => fe
      case None => throw new MongoNotFoundException(s"$source/$id")
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
  def countSequencesBySource(source: SequenceSource): Future[Int] = {
    db.command(Count(collectionName, Some(BSONDocument("proteinRef.source" -> source.value))))
  }

  /**
   * reports sequenceSource to basic stats
   * from mongo
   * db.sequences.aggregate([{$group:{_id:"$proteinRef.source", nbEntries:{$sum : 1}, nbResidues:{$sum:"$length"}}}, {$project:{_id:0, source:"$_id", nbEntries:"$nbEntries", nbResidues:"$nbResidues"}}])
   * @return
   */
  def stats: Future[Map[SequenceSource, SequenceSourceStats]] = {
    val command = RawCommand(BSONDocument(
      "aggregate" -> collectionName,
      "pipeline" -> BSONArray(
        BSONDocument("$group" -> BSONDocument(
          "_id" -> "$proteinRef.source",
          "nbEntries" -> BSONDocument("$sum" -> 1),
          "nbResidues" -> BSONDocument("$sum" -> "$length"))),
        BSONDocument("$project" -> BSONDocument(
          "_id" -> false,
          "source" -> "$_id",
          "nbEntries" -> "$nbEntries",
          "nbResidues" -> "$nbResidues"))
      )
    ))

    val elStats = db.command(command).map({
      doc =>
        doc.getAs[List[BSONDocument]]("result").get.map({elDoc=>
          SequenceSourceStats(SequenceSource(elDoc.getAs[String]("source").get),
            elDoc.getAs[Int]("nbEntries").get,
            elDoc.getAs[Double]("nbResidues").get.toInt)
        })
    })
    elStats.map(_.map(s => (s.source, s)).toMap)
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
