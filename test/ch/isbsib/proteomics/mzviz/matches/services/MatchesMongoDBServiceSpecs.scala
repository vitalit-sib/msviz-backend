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
class MatchesMongoDBServiceSpecs extends Specification with ScalaFutures with TempMongoDBForSpecs {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  sequential

  // create default service
  val service = new MatchMongoDBService(db)


  "empty service" should {
    "counts are 0" in {
      service.countEntries.futureValue must equalTo(0)
      service.countRunIds.futureValue must equalTo(0)
    }
  }

  "import and insert q psm list" should {

    val file_1 = new File("test/resources/mascot/M_100.mzid")
    val file_2 = new File("test/resources/mascot/F001644.mzid")

    "get them up " in {
        service.countEntries.futureValue must equalTo(0)
        service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
        service.countEntries.futureValue must equalTo(62)
        service.insert(LoaderMzIdent.parse(file_2, SearchId("F001644"), RunId("F001644.mgf"), None)._1).futureValue
        service.countEntries.futureValue must equalTo(499)
        service.countRunIds.futureValue must equalTo(2)
    }

  }

  "delete" should {

    "get 2 , remove 1 " in {

      val psmList = service.findAllPSMBySearchId(SearchId("M_100")).futureValue
      psmList.size must equalTo(62)
      service.countEntries.futureValue must equalTo(499)
      service.deleteAllBySearchId(SearchId("M_100")).futureValue
      service.countEntries.futureValue must equalTo(437)
      service.countRunIds.futureValue must equalTo(1)

    }

  }
  "findAllEntriesByRunId" should {
    "find all" in {

      val file_1 = new File("test/resources/mascot/M_100.mzid")
      //insert and check size
      service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100"), RunId("M_100.mgf"), None)._1).futureValue
      val idList = service.findAllSpectrumIdBySearchId(SearchId("M_100")).futureValue
      idList.size must equalTo(62)

      //check JSON content
      val oneEntrie = idList.filter(id => id.id == SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 52.104 min, Period: 1, Cycle(s): 2049 (Experiment 3)"))
      oneEntrie.size mustEqual(1)
      oneEntrie(0).runId must equalTo(RunId("M_100.mgf"))

    }

  }

  "findAllPSMByRunId" should {

      "find all" in {
          val psmList = service.findAllPSMBySearchId(SearchId("M_100")).futureValue
          psmList.size must equalTo(62)
          psmList(0).matchInfo.massDiffUnit.get mustEqual Dalton

      }
  }


  "listProteinRefsBySearchId" should {

      "list all" in {
        val protRefList = service.listProteinRefsBySearchIds(Set(SearchId("M_100"))).futureValue
        protRefList.size must equalTo(27)

        val protRef = protRefList.filter(p => p.AC.value == "CD109_HUMAN")(0)
        protRef.AC mustEqual AccessionCode("CD109_HUMAN")
        protRef.source mustEqual Some(SequenceSource("SwissProt_2014_08.fasta"))
      }
  }


  "listProteinRefsBySearchIdWithModification" should {

      "list all" in {
          val protRefList = service.listProteinRefsBySearchIds(Set(SearchId("M_100")), Option(ModifName("Acetyl"))).futureValue
          protRefList.size must equalTo(1)
          protRefList(0).AC mustEqual AccessionCode("ANXA2_HUMAN")
          protRefList(0).source mustEqual Some(SequenceSource("SwissProt_2014_08.fasta"))
      }


  }

  "findPSMByProtein" should {

      val file_1 = new File("test/resources/mascot/M_100.mzid")

      "list all" in {
          service.insert(LoaderMzIdent.parse(file_1, SearchId("M_100_1"), RunId("M_100.mgf"), None)._1).futureValue

          val psms = service.findAllPSMsByProtein(AccessionCode("CD109_HUMAN")).futureValue
          psms.size must equalTo(4)
      }

      "list all with searchId" in {
          val psms = service.findAllPSMsByProtein(AccessionCode("CD109_HUMAN"), searchIds = Some(Set(SearchId("M_100_1")))).futureValue
          psms.size must equalTo(2)
      }

      "list all AHNK_HUMAN" in {
        val psms = service.findAllPSMsByProtein(AccessionCode("AHNK_HUMAN")).futureValue
        psms.size must equalTo(30)
      }

      "list all AHNK_HUMAN not rejected" in {
        val psms = service.findAllPSMsByProtein(AccessionCode("AHNK_HUMAN"), notRejected = Some(true)).futureValue
        psms.size must equalTo(18)
        psms(0).matchInfo.correctedMoz mustEqual(None)
      }

    }


    "load MXQ data" should {

      "insert and delete" in {
        val oneMQ = LoaderMaxQuant.parse(maxQuantDir = "test/resources/maxquant/", idTitle = Some("yoyo"))(0)
        service.insert(oneMQ._1).futureValue
        val psms = service.findAllPSMsByProtein(AccessionCode("Q9BZF1"), notRejected = Some(true)).futureValue

        // check sizes
        psms.size mustEqual(252)

        // check one PSM
        val psm = psms.filter(_.spectrumId.id.value == "9477")(0)
        psm.matchInfo.modificationProbabilities.get(ModifName("Phospho")) mustEqual("MEGGLADGEPDRT(0.032)S(0.968)LLGDSK")
        psm.matchInfo.highestModifProbability.get(ModifName("Phospho")) mustEqual(0.968)
        psm.matchInfo.correctedMoz mustEqual(Some(1093.46958))

        psm.matchInfo.modificationProbabilities.get(ModifName("Oxidation")) mustEqual("M(1)EGGLADGEPDRTSLLGDSK")
        psm.matchInfo.highestModifProbability.get(ModifName("Oxidation")) mustEqual(1.0)

      }
    }

  "findSpectrumIdWithMolMass" should {

    "none from mascot data" in {
      val spMolMassPair = service.findSpectrumIdWithMolMass(RunId("M_100.mgf")).futureValue
      spMolMassPair.size mustEqual(0)
    }

    "some from maxquant data" in {
      val spMolMassPair = service.findSpectrumIdWithMolMass(RunId("yoyoDMSO")).futureValue
      spMolMassPair.size mustEqual(1187)

      val someData = spMolMassPair.filter(t => t._1.value == "13049")(0)
      someData._1.value mustEqual("13049")
      someData._2.value mustEqual(2188.0688)
    }

  }



}