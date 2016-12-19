package ch.isbsib.proteomics.mzviz.matches.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.{LoaderMaxQuant, LoaderMzIdent}
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class MatchesMongoDBServiceSpecs extends Specification with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mongodDB and add a exp service above it
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

    val file_1 = new File("test/resources/mascot/M_100.mzid")
    val file_2 = new File("test/resources/mascot/F001644.mzid")

    "get them up " in new TempMongoDBService {
        service.countEntries.futureValue must equalTo(0)
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
        service.countEntries.futureValue must equalTo(62)
        service.insert(LoaderMzIdent.parse(file_2, SearchId("F001644"), RunId("F001644.mgf"), None)._1).futureValue
        service.countEntries.futureValue must equalTo(499)
        service.countRunIds.futureValue must equalTo(2)
    }

  }

  "delete" should {

    val file_1 = new File("test/resources/mascot/M_100.mzid")

    "get 2 , remove 1 " in new TempMongoDBService {

        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
        val psmList = service.findAllPSMBySearchId(SearchId("M_100")).futureValue
        psmList.size must equalTo(62)
        service.deleteAllBySearchId(SearchId("M_100")).futureValue
        service.countEntries.futureValue must equalTo(0)
        service.countRunIds.futureValue must equalTo(0)

    }

  }
  "findAllEntriesByRunId" should {
    "find all" in new TempMongoDBService {

        val file_1 = new File("test/resources/mascot/M_100.mzid")

        //insert and check size
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
        val idList = service.findAllSpectrumIdBySearchId(SearchId("M_100")).futureValue
        idList.size must equalTo(62)

        //check JSON content
        idList(0).id must equalTo(SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 50.227 min, Period: 1, Cycle(s): 2033 (Experiment 4)"))
        idList(0).runId must equalTo(RunId("M_100.mgf"))

    }

  }

  "findAllPSMByRunId" should {
      val file_1 = new File("test/resources/mascot/M_100.mzid")

      "find all" in new TempMongoDBService {

          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
          val psmList = service.findAllPSMBySearchId(SearchId("M_100")).futureValue
          psmList.size must equalTo(62)
          psmList(0).matchInfo.massDiffUnit.get mustEqual Dalton

      }
  }


  "listProteinRefsBySearchId" should {

      val file_1 = new File("test/resources/mascot/M_100.mzid")

      "list all" in new TempMongoDBService {
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
          val protRefList = service.listProteinRefsBySearchIds(Set(SearchId("M_100"))).futureValue
          protRefList.size must equalTo(27)
          protRefList(0).AC mustEqual AccessionCode("CD109_HUMAN")
          protRefList(0).source mustEqual Some(SequenceSource("SwissProt_2014_08.fasta"))
      }
  }


  "listProteinRefsBySearchId" should {

      val file_1 = new File("test/resources/mascot/M_100.mzid")

      "list all" in new TempMongoDBService {
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
          val protRefList = service.listProteinRefsBySearchIds(Set(SearchId("M_100"))).futureValue
          protRefList.size must equalTo(27)
          protRefList(0).AC mustEqual AccessionCode("CD109_HUMAN")
          protRefList(0).source mustEqual Some(SequenceSource("SwissProt_2014_08.fasta"))

    }

  }

  "listProteinRefsBySearchIdWithModification" should {

      val file_1 = new File("test/resources/mascot/M_100.mzid")

      "list all" in new TempMongoDBService {
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
          val protRefList = service.listProteinRefsBySearchIds(Set(SearchId("M_100")), Option(ModifName("Acetyl"))).futureValue
          protRefList.size must equalTo(1)
          protRefList(0).AC mustEqual AccessionCode("ANXA2_HUMAN")
          protRefList(0).source mustEqual Some(SequenceSource("SwissProt_2014_08.fasta"))
      }


  }

  "findPSMByProtein" should {

      val file_1 = new File("test/resources/mascot/M_100.mzid")

      "list all" in new TempMongoDBService {
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100_1"), RunId("M_100.mgf"), None)._1).futureValue

          val psms = service.findAllPSMsByProtein(AccessionCode("CD109_HUMAN")).futureValue
          psms.size must equalTo(4)
      }

      "list all with searchId" in new TempMongoDBService {
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100_1"), RunId("M_100.mgf"), None)._1).futureValue

          val psms = service.findAllPSMsByProtein(AccessionCode("CD109_HUMAN"), searchIds = Some(Set(SearchId("M_100_1")))).futureValue
          psms.size must equalTo(2)
      }

      "list all AHNK_HUMAN not rejected" in new TempMongoDBService {
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue

        val psms = service.findAllPSMsByProtein(AccessionCode("AHNK_HUMAN")).futureValue
        psms.size must equalTo(15)
      }

      "list all AHNK_HUMAN not rejected" in new TempMongoDBService {
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue

        val psms = service.findAllPSMsByProtein(AccessionCode("AHNK_HUMAN"), notRejected = Some(true)).futureValue
        psms.size must equalTo(9)
      }

    }

    "load MXQ data" should {

      "insert and delete" in new TempMongoDBService {
        val oneMQ = LoaderMaxQuant.parse(maxQuantDir = "test/resources/maxquant/", idTitle = Some("yoyo"))(0)
        service.insert(oneMQ._1).futureValue
        val psms = service.findAllPSMsByProtein(AccessionCode("Q9BZF1"), notRejected = Some(true)).futureValue

        // check sizes
        service.countEntries.futureValue mustEqual(1231)
        psms.size mustEqual(273)

        // check one PSM
        val psm = psms.filter(_.spectrumId.id.value == "9477")(0)
        psm.matchInfo.modificationProbabilities.get(ModifName("Phospho")) mustEqual("MEGGLADGEPDRT(0.032)S(0.968)LLGDSK")
        psm.matchInfo.highestModifProbability.get(ModifName("Phospho")) mustEqual(0.968)

        psm.matchInfo.modificationProbabilities.get(ModifName("Oxidation")) mustEqual("M(1)EGGLADGEPDRTSLLGDSK")
        psm.matchInfo.highestModifProbability.get(ModifName("Oxidation")) mustEqual(1.0)

      }
    }



}