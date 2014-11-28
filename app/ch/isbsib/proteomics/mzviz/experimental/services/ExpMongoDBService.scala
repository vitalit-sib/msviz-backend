package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, RefSpectrum}
import ch.isbsib.proteomics.mzviz.experimental.{ScanNumber, IdRun, MSRun}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonFormats._
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.core.commands.{GetLastError, RawCommand, Count}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author Alexandre Masselot
 */
class ExpMongoDBService(val db: DefaultDB) {
  val msnSpectraCollectionName = "msnSpectra"

  def msnSpectraCollection: JSONCollection = db.collection[JSONCollection](msnSpectraCollectionName)

  /**
   * TODO insert the indexes here, to begin with something...
   */
  def setupIndexes = {

  }

  setupIndexes

  /**
   * insert an ms run into the database.
   * @param run already parsed and ready
   * @return a Future of the same run (something else might be better)
   */
  def insert(run: MSRun): Future[Int] = {
    val enumerator = Enumerator(run.msnSpectra: _*)
    msnSpectraCollection.bulkInsert(enumerator)
  }

  /**
   * remove all msnSpetra for a given run
   * @param id
   * @return
   */
  def delete(id: IdRun): Future[Unit] = ???

  //{
  //    msnSpectraCollection.remove(BSONDocument("ref.idRun"->id))
  //  }


  /**
   *
   * @param id
   * @return
   */
  def findAllSpectraHeaderByIdRun(id: IdRun): Future[Seq[RefSpectrum]] = ???

  /**
   *
   * @param id
   * @param scan
   * @return
   */
  def findSpectrumByRunIdAndSCanNumber(id: IdRun, scan: ScanNumber): Future[ExpMSnSpectrum] = ???

  /**
   * get the list of the run ids
   * @return
   */
  def listMsRunIds: Future[Seq[IdRun]] = {

    val command = RawCommand(BSONDocument("distinct" -> msnSpectraCollectionName, "key" -> "ref.idRun"))
    db.command(command)
      .map({ doc =>
      doc.getAs[List[String]]("values").get
        .map { i => IdRun(i)}
    })
  }


  /**
   * count the number of Spectra
   * @return
   */
  def countMsnSpectra: Future[Int] = {
    db.command(Count(msnSpectraCollectionName))
  }

  /**
   * count the number of runs
   * TODO there is  a better way to do that directly in mongodb...
   * @return
   */
  def countMsRuns: Future[Int] = {
    listMsRunIds.map(_.size)
  }

  /**
   * a maps with variaous counts (number of spectra, run ...)
   * @return
   */
  def stats: Future[Map[String, Int]] = {
    for {
      nSpectra <- countMsnSpectra
      nRuns <- countMsRuns
    } yield {
      Map("runs" -> nRuns, "msnSpectra" -> nSpectra)

    }
  }

}


object ExpMongoDBService extends Controller with MongoController {
  val default = new ExpMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default


}