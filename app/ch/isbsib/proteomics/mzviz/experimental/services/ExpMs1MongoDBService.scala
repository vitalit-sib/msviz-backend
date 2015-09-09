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

    var inserted: List[Future[Int]] = List()

    while (listMS1.hasNext) {
      // store entries to insert in a list 
      var itEntry: List[Ms1Entry]=List()

      val current = listMS1.next()
      val runID = current.spId.runId
      val rt = current.retentionTime
      current.peaks.foreach {
      peak => val ms1Entry: Ms1Entry = Ms1Entry(runID, rt, peak.intensity, peak.moz)
        itEntry= itEntry :+ ms1Entry
      }
      // inset into db
      val enum=Enumerator.enumerate(itEntry)
      inserted = inserted :+ collection.bulkInsert(enum)
    }

    // give back the sum of all peaks entered
    Future.sequence(inserted).map(_.sum)
  }

  /**
   * retrieve a list of Ms1Entries by moz and tolerance. The list is unsorted.
   * @param moz
   * @param tolerance
   * @return seq of entries
   */

  def findMs1ByRunID_MozAndTol(runId: RunId, moz:Moz,tolerance: Double): Future[List[Ms1Entry]] = {
    val query = Json.obj("moz"->Json.obj("$lte" ->(moz.value + tolerance),"$gte"-> (moz.value - tolerance)))
    collection.find(query).cursor[Ms1Entry].collect[List]()
  }

  /**
   * extract list of intensities and list of moz from list of MS1Entries. Group by retentionTimes and sum the intensities.
   * Then sort by retentionTimes and add 0 values between points which have distances > rtTolerance.
   * @param ms1List
   * @return list of intensities and list of moz
   */

  def extract2Lists(ms1List:Future[List[Ms1Entry]], rtTolerance: Double): Future[JsObject] = {

    // we're in a Future
    ms1List.map(m => {

      // group by retentionTimes
      val rtGroups = m.groupBy(_.rt.value)

      // sum the groups => Map(rt, sum(intensities))
      val summedMap = rtGroups.map({case(k, v) => (k, v.map(_.intensity.value).sum)})

      // sort by rt separate the lists and make a Json-object
      val sortedSums = summedMap.toSeq.sortBy(_._1)

      // helper function to add 0 values
      def addZeroValues(val1:Double, val2:Double, maxDiff:Double):List[(Double,Double)] = {
        if(val1 + maxDiff > val2 - maxDiff){
          List(Tuple2((val1+val2)/2, 0.0))
        }else{
          List(Tuple2(val1 + maxDiff, 0.0), Tuple2(val2 - maxDiff, 0.0))
        }
      }

      // function to add 0 values between peaks which are not close enough
      def checkAndAdd(b:List[(Double, Double)], a:(Double, Double), maxDiff:Double):List[(Double, Double)] = {
        if(b.last._1 + rtTolerance <  a._1){
          b ++ addZeroValues(b.last._1, a._1, maxDiff) :+ a
        } else b :+ a
      }

      val addedMissingRts = sortedSums.drop(1).foldLeft(List(sortedSums(0)))((b,a) => checkAndAdd(b,a, rtTolerance))

       val aux = addedMissingRts.unzip
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


