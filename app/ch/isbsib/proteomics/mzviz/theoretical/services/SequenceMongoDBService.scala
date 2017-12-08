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
  val mainKeyName="""proteinRef.source"""


  setIndexes(List(
    new Index(
      Seq("proteinRef.AC" -> IndexType.Ascending, "proteinRef.source" -> IndexType.Ascending),
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
    * insert a list of Fasta entries sequentially
    * @param entries to be inserted
    * @param blockSize size of entries to be inserted at once
    * @return a Future of the number of entries loaded
    */
  def insertSequentially(entries: Iterator[FastaEntry], blockSize: Int): Future[Int] = {
    val entryBlocks = entries.sliding(blockSize, blockSize)

    val blockResIt:Iterator[Future[Int]] = for (block <- entryBlocks) yield {
      val enumerator = Enumerator.enumerate(block)
      collection.bulkInsert(enumerator)
    }

    blockResIt.foldLeft(Future(0))((a,b) => a.flatMap(a2 => b.map(_ + a2)))
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
   * @param sources data sources
   * @return
   */
  def findEntryByIdentifierAndSources(id: ProteinIdentifier, sources: SequenceSource): Future[FastaEntry] = {
    //Prepare query for multiple sources
    val sourcesArray=sources.toString.split(";")

    val searchFastaEntries = sourcesArray.toStream.map({ source =>
      val query = Json.obj("proteinRef.identifiers" -> id.value, "proteinRef.source" -> source)
      val res2: Future[Option[FastaEntry]]= collection.find(query).cursor[FastaEntry].headOption
      res2
    })

    // check if the entry was found
    val res:Future[Option[FastaEntry]] = Future.find(searchFastaEntries)(_.isDefined) map {_.getOrElse(throw new MongoNotFoundException("could not find Sequence"))}
    res.map(_.getOrElse(throw new MongoNotFoundException("could not find Sequence")))

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
