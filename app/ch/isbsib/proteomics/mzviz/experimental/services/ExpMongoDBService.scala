package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.experimental.{IdRun, MSRun}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonFormats._
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api._
import reactivemongo.core.commands.Count
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

/**
 * @author Alexandre Masselot
 */
class ExpMongoDBService(val db: DefaultDB) {
  def msnSpectraCollection: JSONCollection = db.collection[JSONCollection]("msnSpectra")


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
  def insert(run: MSRun): Future[MSRun] = Future {
    val enumerator = Enumerator(run.msnSpectra: _*)

    msnSpectraCollection.bulkInsert(enumerator)
    run
  }

  /**
   * remove all msnSpetra for a given run
   * @param id
   * @return
   * TODO
   */
  def delete(id: IdRun) = Future[Unit] {

  }

  /**
   * get the list of the run ids
   * @return
   * TODO
   */
  def listMsRunIds: Future[Seq[IdRun]] = Future {
    List(IdRun("bla-123"))
  }

  /**
   * count the number of Spectra
   * @return
   * TODO
   */
  def countMsnSpectra: Future[Int] = {
    db.command(Count("msnSpectra"))
  }

  /**
   * count the number of runs
   * TODO
   * @return
   */
  def countMsRuns: Future[Int] = Future {
    23
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