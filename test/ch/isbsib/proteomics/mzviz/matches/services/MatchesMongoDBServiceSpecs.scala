package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification


/**
 * Created by Roman Mylonas
 */
class MatchesMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs{
    val service = new MatchMongoDBService(db)
  }

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countEntries.futureValue must equalTo(0)
      service.countSources.futureValue must equalTo(0)
    }
  }

  "import and insert q psm list" should {
    "get them up " in new TempMongoDBService {
      println ("get them up ")
      service.countEntries.futureValue must equalTo(0)
      println ("get them up 2")
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid")).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(62)
      println ("get them up 3")
      service.insert(LoaderMzIdent.parse("test/resources/F001644.mzid")).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(499)
      println ("get them up 4")
      service.countSources.futureValue must equalTo(2)
    }
  }

  "delete" should {
    "get 2 , remove 1 " in new TempMongoDBService {
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid")).futureValue
      Thread.sleep(200)
      val psmList = service.findAllEntriesBySource(SpectraSource("rafts1_123spectra")).futureValue
      psmList.size must equalTo(62)

      service.deleteAllBySource(SpectraSource("rafts1_123spectra")).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(0)
      service.countSources.futureValue must equalTo(0)

    }
  }
  "findAllEntriesBySource" should {
    "find all" in new TempMongoDBService {

      println("insert and check size")
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid")).futureValue
      val psmList = service.findAllEntriesBySource(SpectraSource("rafts1_123spectra")).futureValue
      Thread.sleep(200)
      psmList.size must equalTo(62)

      println("check JSON content")
      (psmList(0) \ "spId").as[String] must equalTo("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 50.227 min, Period: 1, Cycle(s): 2033 (Experiment 4)")


    }
  }

}
