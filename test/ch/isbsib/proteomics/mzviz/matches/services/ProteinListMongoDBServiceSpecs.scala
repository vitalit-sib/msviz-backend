package ch.isbsib.proteomics.mzviz.matches.services

import java.io.File
import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.{ParseProteinList, LoaderMzIdent}
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ProteinListMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new ProteinListMongoDBService(db)
  }

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countEntries.futureValue must equalTo(0)
    }
  }

  "import and insert protein list" should {

    val file_1 = new File("test/resources/M_100.mzid")
    val file_2 = new File("test/resources/F001644.mzid")

    val dbInfo_1 = LoaderMzIdent.parseSearchDbSourceInfo(file_1)
    val dbInfo_2 = LoaderMzIdent.parseSearchDbSourceInfo(file_2)

    "get them up " in new TempMongoDBService {
      service.countEntries.futureValue must equalTo(0)
      service.insert(ParseProteinList.parseProtList(file_1, SearchId("M_100"), dbInfo_1)).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(27)
      service.insert(ParseProteinList.parseProtList(file_2, SearchId("F001644"), dbInfo_2)).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(51)
    }
  }

  "delete" should {
    val file_1 = new File("test/resources/M_100.mzid")
    val dbInfo_1 = LoaderMzIdent.parseSearchDbSourceInfo(file_1)

    "get 2 , remove 1 " in new TempMongoDBService {
      service.insert(ParseProteinList.parseProtList(file_1, SearchId("M_100"), dbInfo_1)).futureValue
      Thread.sleep(200)
      val psmList = service.findAllProteinsBySearchId(SearchId("M_100")).futureValue
      psmList.size must equalTo(27)
      service.deleteAllBySearchId(SearchId("M_100")).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(0)

    }
  }

  "insert protein list and find protein identification" should {

    val file_1 = new File("test/resources/M_100.mzid")
    val file_2 = new File("test/resources/F001644.mzid")

    val dbInfo_1 = LoaderMzIdent.parseSearchDbSourceInfo(file_1)
    val dbInfo_2 = LoaderMzIdent.parseSearchDbSourceInfo(file_2)

    "insert and find" in new TempMongoDBService {
      service.countEntries.futureValue must equalTo(0)
      service.insert(ParseProteinList.parseProtList(file_1, SearchId("M_100"), dbInfo_1)).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(27)
      service.insert(ParseProteinList.parseProtList(file_2, SearchId("F001644"), dbInfo_2)).futureValue
      Thread.sleep(200)
      service.countEntries.futureValue must equalTo(51)

      val proteinList = service.findAllProteinsBySearchId(SearchId("F001644")).futureValue
      proteinList.size mustEqual(24)
      proteinList(1).mainProt.proteinAC mustEqual(AccessionCode("Q0IIK2"))

    }

  }





}