package ch.isbsib.proteomics.mzviz.spectrasim.services

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.spectrasim.calcsim.NormDotProdSim
import ch.isbsib.proteomics.mzviz.spectrasim.models.{SpSpRefMatch, SpSpMatch}
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller

/**
 * get similar spectra from the database
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SimilarSpectraMongoDBService (val db: DefaultDB) {

  val expService = new ExpMongoDBService(db)

  /**
   * find similar spectra matches providing a spectrum
   *
   * @param runId
   * @param sp
   * @param scoreThreshold
   * @param ms2PeakMatchTol MS2 peak match tolerance in Daltons
   * @return
   */
  def findSimSpMatches(runId: RunId, sp: ExpMSnSpectrum, scoreThreshold: Double, ms2PeakMatchTol: Double): Future[Seq[SpSpMatch]] = {

    expService.findSpectrumByRunId(runId).map({ spList =>
      spList.map(sp2 => NormDotProdSim().calcSimilarity(sp, sp2, ms2PeakMatchTol))
      .filter(_.score >= scoreThreshold)
    })

  }

  /**
   * find similar spectra matches providing a runId and a spectrum title
   *
   * @param runId
   * @param spTitle
   * @param scoreThreshold
   * @param ms2PeakMatchTol MS2 peak match tolerance in Daltons
   * @return
   */
  def findSimSpRefMatches(runId: RunId, spTitle: String, scoreThreshold: Double, ms2PeakMatchTol: Double): Future[Seq[SpSpRefMatch]] = {

    expService.findSpectrumByRunIdAndTitle(runId, spTitle).flatMap({ sp =>
      findSimSpMatches(runId, sp, scoreThreshold, ms2PeakMatchTol).map({ matches =>
        matches.map(aMatch => SpSpRefMatch(aMatch.sp1.ref, aMatch.sp2.ref, aMatch.score))
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
