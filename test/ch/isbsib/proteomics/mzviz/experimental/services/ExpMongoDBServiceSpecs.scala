package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.ExpPeakMSn
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

class ExpMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new ExpMongoDBService(db)
  }

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)
    }
  }
  "create 2 runs" should {
    "get them up " in new TempMongoDBService {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)

      service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-1")).get).futureValue
      service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-2")).get).futureValue

      service.countMsnSpectra.futureValue must equalTo(246)
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(RunId("test-1"), RunId("test-2")))
    }
  }

  "delete" should {
    "get 2 , remove 1 " in new TempMongoDBService {
      service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-1")).get).futureValue
      service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-2")).get).futureValue
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(RunId("test-1"), RunId("test-2")))

      Thread.sleep(200)
      service.delete(RunId("test-1")).futureValue
      Thread.sleep(200)
      service.countMsRuns.futureValue must equalTo(1)
      service.listMsRunIds.futureValue must equalTo(List(RunId("test-2")))

    }
  }
  "findSpectrumByRunIdAndTitle" should {
    "find one" in new TempMongoDBService {
      val n = service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-1")).get).futureValue

      val sp = service.findSpectrumByRunIdAndTitle(RunId("test-1"), "File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 56.254 min, Period: 1, Cycle(s): 2083 (Experiment 4)").futureValue
      sp.ref.spectrumId.runId must equalTo(RunId("test-1"))
      sp.ref.title must equalTo("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 56.254 min, Period: 1, Cycle(s): 2083 (Experiment 4)")

      sp.peaks must have size 190

      val p2 = sp.peaks(2)
      p2 must equalTo(ExpPeakMSn(Moz(86.0752), Intensity(0.0083), IntensityRank(63), MSLevel(2)))

    }
  }
  "findSpectrumByRunId" should {
    "find one" in new TempMongoDBService {
      val n = service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-1")).get).futureValue

      val sp = service.findSpectrumByRunId(RunId("test-1")).futureValue.toList

      sp.length must equalTo(123)

    }
  }

  "findAllSpectraRefByrunId" should {
    "find all with one runID" in new TempMongoDBService {
      val n = service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("chanclas")).get).futureValue

      val spRefs = service.findAllSpectraRefByrunId(RunId("chanclas")).futureValue.toList
      spRefs must have size (123)
      spRefs(0).spectrumId.runId must equalTo(RunId("chanclas"))

    }

    "find all with one runId in set" in new TempMongoDBService {
      service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("chanclas_0")).get).futureValue
      service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("chanclas_1")).get).futureValue
      service.insert(LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("chanclas_2")).get).futureValue

      val spRefs = service.findAllSpectraRefByrunId(Set(RunId("chanclas_0"), RunId("chanclas_2"))).futureValue.toList
      spRefs must have size (123*2)
      spRefs(0).spectrumId.runId must equalTo(RunId("chanclas_0"))

    }
  }

}

