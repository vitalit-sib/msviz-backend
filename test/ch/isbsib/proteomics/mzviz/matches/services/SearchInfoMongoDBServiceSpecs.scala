package ch.isbsib.proteomics.mzviz.matches.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SearchInfoMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mongodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new SearchInfoDBService(db)
  }

  "insert" should {
    "insert  1" in new TempMongoDBService {
      val searchEntries =LoaderMzIdent.parseSearchInfo(new File("test/resources/M_100.mzid"), SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      inserted mustEqual(1)
    }
  }

  "deleteAllBySearchID" should {
    "remove 1 entry" in new TempMongoDBService {
      val searchEntries =LoaderMzIdent.parseSearchInfo(new File("test/resources/M_100.mzid"), SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      //remove
      val n: Boolean = service.deleteAllBySearchId(SearchId("M_100")).futureValue
      n must equalTo(true)
    }
  }

  "findAllSearchInfoBySearchId" should {
    "find all by searchId" in new TempMongoDBService {

      val searchEntries =LoaderMzIdent.parseSearchInfo(new File("test/resources/M_100.mzid"), SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      val sequenceList = service.findAllSearchInfoBySearchId(SearchId("M_100")).futureValue
      Thread.sleep(200)
      sequenceList.size must equalTo(1)
      print (sequenceList.toString())
      sequenceList.toString() must equalTo("List(SearchInfo(SearchId(M_100),test rafts sample 123 spectra for Roman,List(SearchDatabase(SDB_SwissProt_ID,SwissProt_2014_08.fasta,546238)),roman))")
    }
  }

  "listSearchIds" should {
    "list all" in new TempMongoDBService {
      val searchEntries =LoaderMzIdent.parseSearchInfo(new File("test/resources/M_100.mzid"), SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      val searchIds = service.listSearchIds.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(1)
      searchIds(0) mustEqual SearchId("M_100")
    }
  }


}
