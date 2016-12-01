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
    "insert  true" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)
      val inserted = service.insert(searchEntries).futureValue
      inserted mustEqual(true)
    }
  }

  "deleteAllBySearchID" should {
    "remove 1 entry" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)
      val inserted = service.insert(searchEntries).futureValue
      //remove
      val n: Boolean = service.delete(SearchId("M_100")).futureValue
      n must equalTo(true)
    }
  }

  "get" should {
    "find by searchId" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)
      val inserted = service.insert(searchEntries).futureValue
      val oSearch = service.get(SearchId("M_100")).futureValue
      Thread.sleep(200)
      oSearch.isDefined must equalTo(true)

      val si = oSearch.get
      si.database(0).toString mustEqual("SearchDatabase(SDB_SwissProt_ID,Some(SwissProt_2014_08.fasta),Some(546238))")
      si.enzyme mustEqual("Trypsin")
      si.fragmentTolerance mustEqual("0.3 dalton")
      si.parentTolerance mustEqual(Some("0.3 dalton"))
      si.searchId.value mustEqual("M_100")
      si.title mustEqual("test rafts sample 123 spectra for Roman")
      si.username mustEqual("roman")
    }
  }

  "list" should {
    "all" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)).futureValue
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100_2"), None)).futureValue
      val searchIds = service.list.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(2)
      searchIds(0).searchId mustEqual SearchId("M_100")
      searchIds(1).searchId mustEqual SearchId("M_100_2")
    }
  }

  "listSearchIds" should {
    "list all ids" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)
      val inserted = service.insert(searchEntries).futureValue
      val searchIds = service.listSearchIds.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(1)
      searchIds(0) mustEqual SearchId("M_100")
    }

    "list all ids 2" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)).futureValue
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100_2"), None)).futureValue
      val searchIds = service.listSearchIds.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(2)
      searchIds(0) mustEqual SearchId("M_100")
      searchIds(1) mustEqual SearchId("M_100_2")
    }
  }



  "isSearchIdExist" should {
    "check val" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
      service.isSearchIdExist(SearchId("M_100")).futureValue must equalTo(false)
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)).futureValue
      Thread.sleep(200)
      service.isSearchIdExist(SearchId("M_100")).futureValue must equalTo(true)
    }

    "inserting with duplicate SearchId must throw error" in new TempMongoDBService {
      {
        val mzIdent = scala.xml.XML.loadFile(new File("test/resources/mascot/M_100.mzid"))
        service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)).futureValue
        Thread.sleep(200)
        service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"), None)).futureValue
      } must throwA[Exception]
    }
  }

  "createSearchIdWithError" should {
    "create new error" in new TempMongoDBService {
      service.createSearchIdWithError(SearchId("hoho"), "a new error").futureValue
      val searchList = service.list.futureValue
      searchList.size mustEqual(1)
      searchList(0).status.code mustEqual("error")
      searchList(0).searchId.value.replaceFirst("\\d+", "") mustEqual("_hoho")
    }
  }


}
