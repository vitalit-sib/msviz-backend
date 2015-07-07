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
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      inserted mustEqual(true)
    }
  }

  "deleteAllBySearchID" should {
    "remove 1 entry" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      //remove
      val n: Boolean = service.delete(SearchId("M_100")).futureValue
      n must equalTo(true)
    }
  }

  "get" should {
    "find by searchId" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      val oSearch = service.get(SearchId("M_100")).futureValue
      Thread.sleep(200)
      oSearch.isDefined must equalTo(true)
      oSearch.get.toString() must equalTo("SearchInfo(SearchId(M_100),test rafts sample 123 spectra for Roman,List(SearchDatabase(SDB_SwissProt_ID,SwissProt_2014_08.fasta,546238)),roman,Trypsin,0.3 dalton,0.3 dalton)")
    }
  }

  "list" should {
    "all" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))).futureValue
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100_2"))).futureValue
      val searchIds = service.list.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(2)
      searchIds(0).searchId mustEqual SearchId("M_100")
      searchIds(1).searchId mustEqual SearchId("M_100_2")
    }
  }

  "listSearchIds" should {
    "list all ids" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
      val searchEntries =LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))
      val inserted = service.insert(searchEntries).futureValue
      val searchIds = service.listSearchIds.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(1)
      searchIds(0) mustEqual SearchId("M_100")
    }

    "list all ids 2" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))).futureValue
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100_2"))).futureValue
      val searchIds = service.listSearchIds.futureValue
      Thread.sleep(200)
      searchIds.size must equalTo(2)
      searchIds(0) mustEqual SearchId("M_100")
      searchIds(1) mustEqual SearchId("M_100_2")
    }
  }



  "isSearchIdExist" should {
    "check val" in new TempMongoDBService {
      val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
      service.isSearchIdExist(SearchId("M_100")).futureValue must equalTo(false)
      service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))).futureValue
      Thread.sleep(200)
      service.isSearchIdExist(SearchId("M_100")).futureValue must equalTo(true)
    }

    "inserting with duplicate SearchId must throw error" in new TempMongoDBService {
      {
        val mzIdent = scala.xml.XML.loadFile(new File("test/resources/M_100.mzid"))
        service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))).futureValue
        Thread.sleep(200)
        //        Await.ready(service.insert(LoaderMzIdent.parse("test/resources/M_100.mzid", SearchId("M_100"), RunId("M_100.mgf"))), 300 milli)
        service.insert(LoaderMzIdent.parseSearchInfo(mzIdent, SearchId("M_100"))).futureValue
      } must throwA[Exception]
    }
  }
}
