package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.IdRun
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.ExpPeakMSn
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas
 *
 */

class ExpMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(2, Seconds), interval = Span(500, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs{
    val service = new ExpMongoDBService(db)
  }

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)
    }
  }
//  "create 2 runs" should {
//    "get them up " in new TempMongoDBService {
//      println ("get them up ")
//      service.countMsnSpectra.futureValue must equalTo(0)
//      println ("get them up 1")
//      service.countMsRuns.futureValue must equalTo(0)
//      println ("get them up 2")
//
//      service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-1"))).futureValue
//      service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-2"))).futureValue
//      println ("get them up 6")
//
//      service.countMsnSpectra.futureValue must equalTo(246)
//      println ("get them up 7")
//      service.countMsRuns.futureValue must equalTo(2)
//      println ("get them up 8")
//      service.listMsRunIds.futureValue must equalTo(List(IdRun("test-1"), IdRun("test-2")))
//      println ("get them up 9")
//    }
//  }

  "delete" should {
    "get 2 , remove 1 " in new TempMongoDBService {
      service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-1"))).futureValue
      service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-2"))).futureValue
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(IdRun("test-1"), IdRun("test-2")))

      Thread.sleep(200)
      service.delete(IdRun("test-1")).futureValue
      Thread.sleep(200)
      service.countMsRuns.futureValue must equalTo(1)
      service.listMsRunIds.futureValue must equalTo(List(IdRun("test-2")))

    }
  }
  "findSpectrumByRunIdAndTitle" should {
    "find one" in new TempMongoDBService {
      val n = service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-1"))).futureValue

      val sp = service.findSpectrumByRunIdAndTitle(IdRun("test-1"), "File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 56.254 min, Period: 1, Cycle(s): 2083 (Experiment 4)").futureValue
      sp.ref.idRun must equalTo(Some(IdRun("test-1")))
      sp.ref.title must equalTo("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 56.254 min, Period: 1, Cycle(s): 2083 (Experiment 4)")

      sp.peaks must have size(190)

      val p2 = sp.peaks(2)
      p2 must equalTo(ExpPeakMSn(Moz(307.916800), Intensity(0.1253), IntensityRank(2), MSLevel(2)))

    }
  }
}

