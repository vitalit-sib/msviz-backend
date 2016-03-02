package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpPeakMS1, ExpMs1Spectrum}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMs1BinMongoDBService._
import play.api.libs.json.Json
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.core.commands.LastError
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import ch.isbsib.proteomics.mzviz.commons.RetentionTime
import scala.collection.mutable.ListBuffer

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1TmpMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "ms1Tmp"
  val mainKeyName = "ref"

  new Index(
    Seq("ref" -> IndexType.Ascending),
    name = Some("ref")
  )


  /**
   * insert a list of MS1 spectra into a temporary collection "ms1Tmp"
   *
   * @param intensityThreshold all peaks below this threshold will be ignored
   * @param ms1Iterator ExpMs1Spectum
   * @return gives back the number of entries inserted into the db
   */
  def insertMs1spectra(ms1Iterator: Iterator[ExpMs1Spectrum], intensityThreshold:Double): Future[Int] = {


    val nrList: ListBuffer[Future[Int]] = ListBuffer()

    while(ms1Iterator.hasNext){
      val sp = ms1Iterator.next()
      nrList.append(insertMs1spectrum(sp, intensityThreshold))
    }

    Future.sequence(nrList.toList).map(_.sum)
  }



  /**
   * insert a MS1 spectrum into a temporary collection "ms1Tmp"
   *
   * @param intensityThreshold all peaks below this threshold will be ignored
   * @param ms1 ExpMs1Spectum
   * @return gives back the number of entries inserted into the db
   */
  def insertMs1spectrum(ms1: ExpMs1Spectrum, intensityThreshold:Double): Future[Int] = {

      Future.sequence(ms1.peaks.map({peak =>

        if(peak.intensity.value >= intensityThreshold){
          insertMs1peak(peak, ms1.spId.runId, ms1.retentionTime)
        }else{
          Future{0}
        }

      })).map(_.sum)

  }



  def insertMs1peak(ms1Peak: ExpPeakMS1, runId: RunId, rt: RetentionTime): Future[Int] = {

    // prepare the reference (of type SearchId_Moz, e.g. F003965_487)
    val ref = runId.value + "_" + ms1Peak.moz.value.toInt.toString
    val selector = Json.obj("ref" -> ref)

    // add a new peak
    val newEntry = Json.obj("$push" -> Json.obj(
                                        "moz" -> ms1Peak.moz.value,
                                        "int" -> ms1Peak.intensity.value,
                                        "rt" -> rt.value
                                        )
    )

    // create or update
    val answer = collection.update(selector, newEntry, upsert = true)

    answer.map({
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => 1
    })

  }

}


/**
 * the companion object
 */
object ExpMs1TmpMongoDBService extends Controller with MongoController {
  val default = new ExpMs1TmpMongoDBService(db)

  /**
   * get the default db/collection
   * @return
   */
  def apply() = default

}