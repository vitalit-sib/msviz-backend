package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, ExpMs1Spectrum}
import play.api.Play
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

/**
  * @author Roman Mylonas
  *         copyright 2016-2017, SIB Swiss Institute of Bioinformatics
  */
class ExpMs1AndMs2MongoDBService(val db: DefaultDB) {

  val ms1Service = new ExpMs1BinMongoDBService(db)
  val msnService = new ExpMongoDBService(db)


  /**
    * Insert all spectra (MS1 or MSn) from an iterator using a given buffer size
    * @param itMs1Ms2
    * @param runId
    * @return
    */
  def insertMs1And2spectra(itMs1Ms2: Iterator[Either[ExpMs1Spectrum, ExpMSnSpectrum]], runId: RunId, ms1IntensityThreshold: Double): Future[Boolean] = {
    // number of spectra which are parsed before inserting
    val bufferSize =  if(Play.maybeApplication.isDefined){
      Play.current.configuration.getString("experimental.ms1and2.buffer").get.toInt
    } else 50

    // timeout in minutes for inserting one buffer of ms1 data
    val dbTimeout = if(Play.maybeApplication.isDefined){
      Play.current.configuration.getString("experimental.db.timeout").get.toInt
    } else 10

    // split the iterator into slices
    val slidingIt = itMs1Ms2.sliding(bufferSize, bufferSize)

    // loop through all slices
    val resIt:Iterator[Future[Boolean]] = for (slice <- slidingIt) yield {

      val itMs1Ms2parts: (Seq[Either[ExpMs1Spectrum, ExpMSnSpectrum]], Seq[Either[ExpMs1Spectrum, ExpMSnSpectrum]]) = slice.partition(_.isLeft)

      val ms1List: Seq[ExpMs1Spectrum] = itMs1Ms2parts._1.map(_.left.get)
      val ms2List: Seq[ExpMSnSpectrum] = itMs1Ms2parts._2.map(_.right.get)

      // insert ms1
      val ms1Res: Future[Boolean] = ms1Service.insertMs1Slice(ms1List, ms1IntensityThreshold, dbTimeout)
      val ms2Res: Future[Int] = msnService.insertMs2slice(ms2List, runId)

      ms1Res.flatMap(ms1 => ms2Res.map(ms2 => ms2 > 0 & ms1))
    }

    resIt.foldLeft(Future{true})((a,b) => a.flatMap(a2 => b.map(b2 => a2 & b2)))
  }

}


/**
  * the companion object
  */
object ExpMs1AndMs2MongoDBService extends Controller with MongoController {
  val default = new ExpMs1AndMs2MongoDBService(db)

  /**
    * get the default db/collection
    * @return
    */
  def apply() = default

}