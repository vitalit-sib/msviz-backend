package ch.isbsib.proteomics.mzviz.matches.services

import java.io.File
import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import play.api.test.Helpers._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ProteinMatchMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new ProteinMatchMongoDBService(db)
  }

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countEntries.futureValue must equalTo(0)
    }
  }

  "import and insert protein list" should {

    val file_1 = new File("test/resources/mascot/M_100.mzid")
    val file_2 = new File("test/resources/mascot/F001644.mzid")

    "get them up " in new TempMongoDBService {

        service.countEntries.futureValue must equalTo(0)
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("1"))._2).futureValue
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(27)
        service.insert(LoaderMzIdent.parse(file_2, SearchId("F001644"), RunId("2"))._2).futureValue
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(51)
      }

  }

  "delete" should {
    val file_1 = new File("test/resources/mascot/M_100.mzid")

    "get 2 , remove 1 " in new TempMongoDBService {

        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("1"))._2).futureValue
        Thread.sleep(200)
        val psmList = service.findAllProteinsBySearchId(SearchId("M_100")).futureValue
        psmList.size must equalTo(27)
        service.deleteAllBySearchId(SearchId("M_100")).futureValue
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(0)
      }

  }

  "insert protein list and find protein identification" should {

    val file_1 = new File("test/resources/mascot/M_100.mzid")
    val file_2 = new File("test/resources/mascot/F001644.mzid")

    "insert and find" in new TempMongoDBService {

        service.countEntries.futureValue must equalTo(0)
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("1"))._2).futureValue
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(27)
        service.insert(LoaderMzIdent.parse(file_2, SearchId("F001644"), RunId("2"))._2).futureValue
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(51)

        val proteinList = service.findAllProteinsBySearchId(SearchId("F001644")).futureValue
        proteinList.size mustEqual (24)
        proteinList(1).mainProt.proteinAC mustEqual (AccessionCode("Q0IIK2"))

        val proteinListAll = service.findAllProteinsBySearchIds(Set(SearchId("F001644"), SearchId("M_100"))).futureValue
        proteinListAll.size mustEqual (51)

        val proteinListBySearchAndAc = service.findAllProteinsBySearchIdsAndACs(Set(SearchId("M_100")), Set(AccessionCode("AHNK_HUMAN"), AccessionCode("VIME_HUMAN"))).futureValue
        proteinListBySearchAndAc.size mustEqual (2)
      }

  }

  "delete multiple sources" should {

    val file_1 = new File("test/resources/mascot/M_100.mzid")
    val file_2 = new File("test/resources/mascot/F001644.mzid")

    "insert and delete" in new TempMongoDBService {

        service.countEntries.futureValue must equalTo(0)
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("1"))._2).futureValue
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(27)
        service.insert(LoaderMzIdent.parse(file_2, SearchId("F001644"), RunId("2"))._2).futureValue
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(51)

        service.deleteAllBySearchIds(Set(SearchId("M_100"), SearchId("F001644")))
        Thread.sleep(200)
        service.countEntries.futureValue must equalTo(0)
      }

  }

}