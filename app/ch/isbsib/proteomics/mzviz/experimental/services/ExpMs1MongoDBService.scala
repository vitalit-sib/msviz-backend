package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.{Moz, Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumRef, Ms1Entry, ExpMs1Spectrum}
import org.specs2.execute.Success
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Controller
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.core.commands.{Count, LastError}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Failure

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1MongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "ms1Spectra"
  val mainKeyName = "ref"

//  setIndexes(List(
//    new Index(
//      Seq("ref" -> IndexType.Ascending),
//      name = Some("ref")),
//   new Index(
//      Seq("runId" -> IndexType.Ascending, "intensity" -> IndexType.Ascending),
//      name = Some("intensity"),
//      unique = true)
//  ))
  new Index(
    Seq("ref" -> IndexType.Ascending),
    name = Some("ref")
  )



  /**
   * insert every entry with RunId, rt, intensity and moz
   * @param listMS1, iterator of MS1
   * @return number of entries
   */

  def insertListMS1(listMS1: Iterator[ExpMs1Spectrum]): Future[Int] ={

    var itEntry: List[Ms1Entry]=List()
    while (listMS1.hasNext) {
      val current = listMS1.next()
      val runID = current.spId.runId
      val rt = current.retentionTime
      current.peaks.foreach {
      peak => val ms1Entry: Ms1Entry = Ms1Entry(runID, rt, peak.intensity, peak.moz)
        collection.insert(ms1Entry)
        itEntry= itEntry ::: List(ms1Entry)
      }
    }
    val enum=Enumerator(itEntry)
    return collection.bulkInsert(enum)
  }
/*
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
*/
  /**
   * retrieves  by moz and tolerance
   * @param moz
   * @param tolerance
   * @return seq of entries
   */

  def findMs1ByRunID_MozAndTol(runId: RunId, moz:Moz,tolerance: Double): Future[List[Ms1Entry]] = {
    val query = Json.obj("moz"->Json.obj("$lte" ->(moz.value + tolerance),"$gte"-> (moz.value - tolerance)))
    collection.find(query).cursor[Ms1Entry].collect[List]()
  }

  /**
   * extract list of intensities and list of moz from list of MS1Entries
   * @param ms1List
   * @return list of intensities and list of moz
   */

  def extract2Lists(ms1List:Future[List[Ms1Entry]]): Future[JsObject] = {


    ms1List.map(m => {
      val aux = m.map(a =>
      (a.rt.value, a.intensity.value)).unzip
      Json.obj("rt" ->aux._1, "intensities" -> aux._2)
    })
  }

  /**
   * remove all Ms1Entry for a given run
   * @param runId the run id
   * @return
   */
  def delete(runId: RunId): Future[Boolean] = {
    val query = Json.obj("ref" -> runId.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }


  /**
   * retrieves  by run id
   * @param runId the run id
   * @return seq of entries
   */


  def findMs1ByRunId(runId: RunId): Future[Iterator[Ms1Entry]] = {
    val query = Json.obj("ref" -> runId.value)
    collection.find(query).cursor[Ms1Entry].collect[Iterator]()
  }



  /**
   * count the number of Spectra
   * @return
   */
  def countMsnSpectra: Future[Int] = {
    db.command(Count(collectionName))
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


