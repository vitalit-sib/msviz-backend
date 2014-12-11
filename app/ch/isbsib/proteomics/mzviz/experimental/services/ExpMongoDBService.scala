package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoDBService, MongoNotFoundException}
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, RefSpectrum}
import ch.isbsib.proteomics.mzviz.experimental.{ScanNumber, IdRun, MSRun}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
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
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
class ExpMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "msnSpectra"

  setIndexes(List(new Index(
    Seq("ref.idRun" -> IndexType.Ascending, "ref.title" -> IndexType.Ascending),
    name = Some("idRun_title"),
    unique = true)))


  /**
   * insert an ms run into the database.
   * @param run already parsed and ready
   * @return a Future of the same run (something else might be better)
   */
  def insert(run: MSRun): Future[Int] = {
    val enumerator = Enumerator(run.msnSpectra: _*)
    collection.bulkInsert(enumerator)
  }

  /**
   * remove all msnSpectra for a given run
   * @param idRun the run id
   * @return
   */
  def delete(idRun: IdRun): Future[Boolean] = {
    val query = Json.obj("ref.idRun" -> idRun.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   *
   * @param idRun the run id
   * @return
   */
  def findAllRefSpectraByIdRun(idRun: IdRun): Future[Seq[JsObject]] = {
    val query = Json.obj("ref.idRun" -> idRun.value)
    val projection = Json.obj("ref" -> 1, "_id" -> 1)
    collection.find(query, projection).cursor[JsObject].collect[List]()
  }

  /**
   * retrieves  by run & spectra title (unique by index setup)
   * @param idRun the run id
   * @param title the spectrum title
   * @return
   */
  def findSpectrumByRunIdAndTitle(idRun: IdRun, title: String): Future[ExpMSnSpectrum] = {
    val query = Json.obj("ref.idRun" -> idRun.value, "ref.title" -> title)
    collection.find(query).cursor[ExpMSnSpectrum].headOption map {
      case Some(sp: ExpMSnSpectrum) => sp
      case None => throw new MongoNotFoundException(s"${idRun.value}/$title")
    }
  }

  /**
   * get the list of the run ids
   * @return
   */
  def listMsRunIds: Future[Seq[IdRun]] = {
    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "ref.idRun"))
    db.command(command)
      .map({
      doc =>
        doc.getAs[List[String]]("values").get
          .map {
          i => IdRun(i)
        }
    })
  }


  /**
   * count the number of Spectra
   * @return
   */
  def countMsnSpectra: Future[Int] = {
    db.command(Count(collectionName))
  }

  /**
   * count the number of runs
   * @return
   */
  def countMsRuns: Future[Int] = {
    listMsRunIds.map(_.size)
  }

  /**
   * a maps with various counts (number of spectra, run ...)
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


