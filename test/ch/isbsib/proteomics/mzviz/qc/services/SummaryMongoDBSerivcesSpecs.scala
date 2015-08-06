package ch.isbsib.proteomics.mzviz.qc.services

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.qc.importer.LoadSummary
import org.specs2.mutable.Specification
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import ch.isbsib.proteomics.mzviz.qc.services.JsonQCFormats._

/**
 * Created by qjolliet on 05/08/15.
 */
class SummaryMongoDBSerivcesSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mongo database and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new SummaryMongoDBServices(db)
  }


  "insert" should {
    "insert  2" in new TempMongoDBService {
      val entries = LoadSummary("./test/resources/qc/summary.txt").getSummaryEntry
      val n: Int = service.insert(entries).futureValue
      n must equalTo(2)
    }
  }

}
