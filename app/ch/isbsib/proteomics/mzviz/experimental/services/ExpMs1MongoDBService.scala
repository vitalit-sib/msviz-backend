package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.{Moz, Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumRef, Ms1Entry, ExpMs1Spectrum}
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Controller
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.core.commands.LastError
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._

import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1MongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "ms1Spectra"
  val mainKeyName = "ref.spectrumId.runId"

  setIndexes(List(
    new Index(
      Seq("ref.spectrumId.runId" -> IndexType.Ascending), // "ref.title" -> IndexType.Ascending),
      name = Some("runId")),
    new Index(
      Seq("ref.spectrumId.runId" -> IndexType.Ascending, "ref.spectrumId.id" -> IndexType.Ascending), // "ref.title" -> IndexType.Ascending),
      name = Some("ref.spectrumId"),
      unique = true)
  ))


  /**
   * insert every entry with RunId, rt, intensity and moz
   * @param listMS1, iterator of MS1
   * @return number of entries
   */

  def insertListMS1(listMS1: Iterator[ExpMs1Spectrum]) {

    if (listMS1.hasNext) {
      val current = listMS1.next()

      val runID = current.spId.runId
      val rt = current.retentionTime
      current.peaks.foreach {
      peak => val ms1Entry: Ms1Entry = Ms1Entry(runID, rt, peak.intensity, peak.moz)
        collection.insert(ms1Entry)
      }
    }
  }

  /**
   * insert every entry with RunId, rt, intensity and moz
   * @param listMS1, iterator of MS1
   * @return number of entries
   */

  def insertListMS12(listMS1: Iterator[ExpMs1Spectrum]): Future[Int] = {

    var myList = new ListBuffer[Ms1Entry]()

    if (listMS1.hasNext) {
      val current = listMS1.next()

      val runID = current.spId.runId
      val rt = current.retentionTime
      current.peaks.foreach {
        peak => val ms1Entry: Ms1Entry = Ms1Entry(runID, rt, peak.intensity, peak.moz)
          myList += (ms1Entry)
      }
    }
    val enum = Enumerator(myList)
    collection.bulkInsert(enum)
  }

  /**
   * retrieves  by moz and tolerance
   * @param moz
   * @param tolerance
   * @return seq of entries
   */

//TOCHANGE!!!!
  def findMs1ByRunID_MozAndTol(runId: RunId, moz:Moz,tolerance: Double): Future[Seq[Ms1Entry]] = {
    val query = Json.obj("ref.runId" -> runId.value, "ref.moz"-> Json.obj("$lte" -> (moz.value + tolerance)),"ref.moz"-> Json.obj("$gte" -> (moz.value - tolerance)))
    collection.find(query)
      .cursor[JsObject]
      .collect[List]()
      .map(lo => lo.map({ o =>
      Json.fromJson[Ms1Entry](o \ "ref").asOpt.get
    }))
  }


  /**
   * retrieves  by run id
   * @param runId the run id
   * @return seq of entries
   */


  def findMs1ByRunId(runId: RunId): Future[Ms1Entry] = {
    val query = Json.obj("ref.runId" -> runId.value)
    collection.find(query).cursor[Ms1Entry].headOption map {
      case Some(sp: Ms1Entry) => sp
      case None => throw new MongoNotFoundException(s"${runId.value}")
    }
  }

  /**
   * remove all Ms1Entry for a given run
   * @param runId the run id
   * @return
   */
  def delete(runId: RunId): Future[Boolean] = {
    val query = Json.obj("ref.runId" -> runId.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }
}

object ExpMs1MongoDBService extends Controller with MongoController {
  val default = new ExpMs1MongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default
}


