package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBServiceForSpecs
import ch.isbsib.proteomics.mzviz.experimental.IdRun
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.execute.AsResult
import org.specs2.mutable.{Around, Specification}
import reactivemongo.api.MongoDriver

import scala.util.Random

/**
 * @author Roman Mylonas
 *
 *         TODO change the write concern on the test database
 */

class ExpMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(2, Seconds), interval = Span(5, Millis))



  "empty service" should {
    "counts are 0" in new TempMongoDBServiceForSpecs {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)
    }
  }
  "create 2 runs" should {
    "get them up " in new TempMongoDBServiceForSpecs {
      //      "solve the destory db firest" >> pending {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)

      val n = service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-1"))).futureValue

      service.countMsnSpectra.futureValue must equalTo(123)
      service.countMsRuns.futureValue must equalTo(1)

      service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-2"))).futureValue

      service.countMsnSpectra.futureValue must equalTo(246)
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(IdRun("test-1"), IdRun("test-2")))
    }
    //    }
  }

}

