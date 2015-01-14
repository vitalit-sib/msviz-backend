package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
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
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new MatchMongoDBService(db)
  }

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countEntries.futureValue must equalTo(0)
      service.countRunIds.futureValue must equalTo(0)
    }
  }

  "import and insert q psm list" should {
    "get them up " in new TempMongoDBService {
      service.countEntries.futureValue must equalTo(0)
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100"), RunId("M_100.mgf"))).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(62)
      service.insert(LoaderMzIdent.parse("test/resources/F001644.mzid", SearchId("F001644"), RunId("F001644.mgf"))).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(499)
      service.countRunIds.futureValue must equalTo(2)
    }
  }

  "delete" should {
    "get 2 , remove 1 " in new TempMongoDBService {
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100"), RunId("M_100.mgf"))).futureValue
      Thread.sleep(200)
      val psmList = service.findAllPSMBySearchId(SearchId("M_100")).futureValue
      psmList.size must equalTo(62)
      service.deleteAllBySearchId(SearchId("M_100")).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(0)
      service.countRunIds.futureValue must equalTo(0)

    }
  }
  "findAllEntriesByRunId" should {
    "find all" in new TempMongoDBService {

      println("insert and check size")
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100"), RunId("M_100.mgf"))).futureValue
      val idList = service.findAllSpectrumIdBySearchId(SearchId("M_100")).futureValue
      Thread.sleep(200)
      idList.size must equalTo(62)

      println("check JSON content")
      idList(0).id must equalTo(SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 50.227 min, Period: 1, Cycle(s): 2033 (Experiment 4)"))
      idList(0).runId must equalTo(RunId("M_100.mgf"))
    }
  }

  "findAllPSMByRunId" should {
    "find all" in new TempMongoDBService {

      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100"), RunId("M_100.mgf"))).futureValue
      val psmList = service.findAllPSMBySearchId(SearchId("M_100")).futureValue
      Thread.sleep(200)
      psmList.size must equalTo(62)
    }
  }


  "listSearchIds" should {
    "list all" in new TempMongoDBService {
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100"), RunId("M_100.mgf"))).futureValue
      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100_2"), RunId("M_100.mgf"))).futureValue
      val searchIds = service.listSearchIds.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(2)
      searchIds(0) mustEqual (SearchId("M_100"))
      searchIds(1) mustEqual (SearchId("M_100_2"))
    }
  }

  "listProteinRefsBySearchId" should {
    "list all" in new TempMongoDBService {

      service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100"), RunId("M_100.mgf"))).futureValue
      val protRefList = service.listProteinRefsBySearchId(SearchId("M_100")).futureValue
      Thread.sleep(200)
      protRefList.size must equalTo(27)
      protRefList(0).AC mustEqual (AccessionCode("CD109_HUMAN"))
      protRefList(0).source mustEqual (Some(SequenceSource("TODO")))
    }
  }

}
