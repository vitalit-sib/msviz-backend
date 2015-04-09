package ch.isbsib.proteomics.mzviz.spectrasim.services

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.spectrasim.calcsim.NormDotProdSim
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpMatch
import reactivemongo.api.DefaultDB
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._

/**
 * get similar spectra from the database
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SimilarSpectraMongoDBService (val db: DefaultDB){

  def findSimilarSpectra(runId: RunId, sp: ExpMSnSpectrum, similarityThreshold: Double, ms2PeakMatchTol: Double): Future[Seq[SpSpMatch]] = {

    val service = new ExpMongoDBService(db)

    service.findSpectrumByRunId(runId).map({ spList =>
      spList.map(sp2 => NormDotProdSim().calcSimilarity(sp, sp2, ms2PeakMatchTol))
      .filter(_.similarity >= similarityThreshold)
    })

  }
}
