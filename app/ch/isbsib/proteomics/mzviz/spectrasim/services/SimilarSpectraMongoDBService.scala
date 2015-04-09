package ch.isbsib.proteomics.mzviz.spectrasim.services

import ch.isbsib.proteomics.mzviz.commons.services.MongoDBService
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.spectrasim.calcsim.NormDotProdSim
import ch.isbsib.proteomics.mzviz.spectrasim.models.{SpSpRefMatch, SpSpMatch}
import ch.isbsib.proteomics.mzviz.spectrasim.services.SimilarSpectraMongoDBService
import play.api.libs.json.{JsValue, Json, JsObject}
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import ch.isbsib.proteomics.mzviz.spectrasim.services.JsonSimFormats._
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import play.api.mvc.Controller

/**
 * get similar spectra from the database
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SimilarSpectraMongoDBService (val db: DefaultDB) {

  val expService = new ExpMongoDBService(db)

  def findSimSpMatches(runId: RunId, sp: ExpMSnSpectrum, scoreThreshold: Double, ms2PeakMatchTol: Double): Future[Seq[SpSpMatch]] = {

    expService.findSpectrumByRunId(runId).map({ spList =>
      spList.map(sp2 => NormDotProdSim().calcSimilarity(sp, sp2, ms2PeakMatchTol))
      .filter(_.score >= scoreThreshold)
    })

  }

  def findSimSpRefMatches(runId: RunId, spTitle: String, scoreThreshold: Double, ms2PeakMatchTol: Double): Future[Seq[JsValue]] = {

    val spSpMatches = for{
      sp <- expService.findSpectrumByRunIdAndTitle(runId, spTitle)
    }yield {
        findSimSpMatches(runId, sp, scoreThreshold, ms2PeakMatchTol)
    }

    spSpMatches.flatMap({ spMatches1 =>
      spMatches1.map({ spMatches2 =>
        spMatches2.map(aMatch => Json.toJson(SpSpRefMatch(aMatch.sp1.ref, aMatch.sp2.ref, aMatch.score)))
      })
    })

  }

}

object SimilarSpectraMongoDBService extends Controller with MongoController {
  val default = new SimilarSpectraMongoDBService(db)

  /**
   * get the default db/collection
   * @return
   */
  def apply() = default

}
