package ch.isbsib.proteomics.mzviz.spectrasim.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpMatch
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SimilarSpectraMongoDBServiceSpecs extends Specification with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */

  trait TempDBService extends TempMongoDBForSpecs {
    val expService = new ExpMongoDBService(db)
    val simService = new SimilarSpectraMongoDBService(db)
  }

  "findSpectrumByRunId" should {
    "find one" in new TempDBService {

      // prepare experimental data
      val n = expService.insert(LoaderMGF.load(new File("test/resources/M_100.mgf"), RunId("test-1")).get).futureValue
      val sp = expService.findSpectrumByRunIdAndTitle(RunId("test-1"), "File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 56.254 min, Period: 1, Cycle(s): 2083 (Experiment 4)").futureValue

      val spSpMatches = simService.findSimilarSpectra(RunId("test-1"), sp, 0.1, 0.5).futureValue

      spSpMatches.length must equalTo(18)

      // best match should be search spectrum
      def maxMatch(match1: SpSpMatch, match2: SpSpMatch): SpSpMatch = if(match1.similarity > match2.similarity) match1 else match2
      val bestMatch = spSpMatches.reduceLeft(maxMatch)

      sp must equalTo(bestMatch.sp2)

    }
  }

}
