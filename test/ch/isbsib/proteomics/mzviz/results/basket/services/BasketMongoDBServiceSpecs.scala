package ch.isbsib.proteomics.mzviz.results.basket.services

import java.util.Calendar

import ch.isbsib.proteomics.mzviz.commons.services.MongoId
import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.results.basket.BasketMongoDBService
import ch.isbsib.proteomics.mzviz.results.basket.models.{BasketEntry, RtRange, XicPeak}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class BasketMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new BasketMongoDBService(db)
  }

  /**
    test data
    */

  val entry1 = new BasketEntry(None, proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "SLIWTLLK",
    startPos = 405,
    endPos = 412,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7371A.8585.8585.2"), runId = RunId("F002453")),
    score = 87.5,
    localizationScore = Some(100),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 35, upperRt = 39),
    rtSelected = RtRange(lowerRt = 36, upperRt = 37),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),Some(RetentionTime(36.48)),Some(Intensity(198000))), XicPeak(SearchId("F002453"), None, None)),
    creationDate = Some(Calendar.getInstance().getTime())
  )

  val entry2 = new BasketEntry(None, proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "SLIWT{Phospho}LLK",
    startPos = 405,
    endPos = 412,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7371A.12161.12161.2"), runId = RunId("F002453")),
    score = 87.5,
    localizationScore = Some(100),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 47.5, upperRt = 48.5),
    rtSelected = RtRange(lowerRt = 47.8, upperRt = 48.2),
    xicPeaks = Seq(XicPeak(SearchId("F002453"), Some(RetentionTime(47.93)), Some(Intensity(472000))), XicPeak(SearchId("F002453"), Some(RetentionTime(47.94)), Some(Intensity(1470000)))),
    creationDate = Some(Calendar.getInstance().getTime())
  )

  val entry3 = new BasketEntry(_id=None, proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "ANNLHSGDN{Deamidated}FQLNDS{Phospho}EIER",
    startPos = 300,
    endPos = 318,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7374A.9189.9189.3"), runId = RunId("F002454")),
    score = 87.5,
    localizationScore = Some(100),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 36.5, upperRt = 39.5),
    rtSelected = RtRange(lowerRt = 37.5, upperRt = 38.0),
    xicPeaks = Seq(XicPeak(SearchId("F002453"), Some(RetentionTime(37.74)), Some(Intensity(139000))), XicPeak(SearchId("F002453"), Some(RetentionTime(37.82)), Some(Intensity(634000)))),
    creationDate = Some(Calendar.getInstance().getTime())

  )

  val entry4 = new BasketEntry(_id=None, proteinAC = AccessionCode("K2C1_HUMAN"),
    peptideSeq = "MS{Phospho}GEC{Carbamidomethyl}APN{Deamidated}VSVSVSTSHTTISGGGSR",
    startPos = 493,
    endPos = 518,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7374A.6984.6984.3"), runId = RunId("F002454")),
    score = 87.5,
    localizationScore = Some(100),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 29.5, upperRt = 31.5),
    rtSelected = RtRange(lowerRt = 30.0, upperRt = 30.5),
    xicPeaks = Seq(XicPeak(SearchId("F002453"), Some(RetentionTime(30.51)), Some(Intensity(95400))), XicPeak(SearchId("F002453"), Some(RetentionTime(30.30)), Some(Intensity(3620000)))),
    creationDate = Some(Calendar.getInstance().getTime())
  )

  val entry5 = new BasketEntry(_id=None, proteinAC = AccessionCode("K2C1_HUMAN"),
    peptideSeq = "MS{Phospho}GEC{Carbamidomethyl}APN{Deamidated}VSVSVSTSHTTISGGGSR",
    startPos = 493,
    endPos = 518,
    searchIds = "F009998,F009999",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7374A.6984.6984.3"), runId = RunId("F009999")),
    score = 87.5,
    localizationScore = Some(100),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 29.5, upperRt = 31.5),
    rtSelected = RtRange(lowerRt = 30.0, upperRt = 30.5),
    xicPeaks = Seq(XicPeak(SearchId("F009998"), Some(RetentionTime(30.51)), Some(Intensity(95400))), XicPeak(SearchId("F009999"), Some(RetentionTime(30.30)), Some(Intensity(3620000)))),
    creationDate = Some(Calendar.getInstance().getTime())
  )


  /**
  tests
    */

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countBasketEntries.futureValue must equalTo(0)
      service.listProteins("1,2").futureValue.length must equalTo(0)
    }
  }

  "create basket entries" should {
    "create 5 and count" in new TempMongoDBService {
      service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      service.countBasketEntries.futureValue must equalTo(5)
      service.listProteins("1,2").futureValue.length must equalTo(0)
      val proteinList = service.listProteins("F002453,F002454").futureValue
      proteinList.length mustEqual(2)
      //proteinList(0) must equalTo(AccessionCode("OSBL8_HUMAN"))
    }
  }
  // @TODO this test is diseabled since the mongodb on the test server does not support the "$text -> $search" commmand
//  "create and delete" should {
//    "create 5 and delete all" in new TempMongoDBService {
//      service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
//      service.countBasketEntries.futureValue must equalTo(5)
//      service.deleteBySearchId("F002453").futureValue mustEqual(true)
//      service.countBasketEntries.futureValue must equalTo(1)
//    }
//  }

    "create and delete by MongoId" should {
      "create 5 and delete one" in new TempMongoDBService {
        service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
        service.countBasketEntries.futureValue must equalTo(5)
        val basketEntries = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
        basketEntries.length mustEqual(3)
        val mongoId = basketEntries(0)._id
        mongoId.isEmpty mustEqual(false)
        service.deleteByMongoId(mongoId.get.$oid).futureValue mustEqual(true)
        val basketEntriesAfterDelete = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
        basketEntriesAfterDelete.length mustEqual(2)
        service.countBasketEntries.futureValue must equalTo(4)
      }
    }

  "create and delete by SearchId" should {
    "create 5 and delete 4" in new TempMongoDBService {
      service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      service.countBasketEntries.futureValue must equalTo(5)
      val basketEntries = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntries.length mustEqual(3)
      service.deleteBySearchId(Set(SearchId("F002453"))).futureValue mustEqual(1)
      val basketEntriesAfterDelete1 = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntriesAfterDelete1.length mustEqual(0)
      service.countBasketEntries.futureValue must equalTo(1)
    }
  }

  "create and delete by BasketId" should {
    "create 5 and delete 4" in new TempMongoDBService {
      service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      service.countBasketEntries.futureValue must equalTo(5)
      val basketEntries = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntries.length mustEqual(3)
      service.deleteByBasketId("F002453,F002454").futureValue mustEqual(true)
      val basketEntriesAfterDelete1 = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntriesAfterDelete1.length mustEqual(0)
      service.countBasketEntries.futureValue must equalTo(1)
    }
  }

  "list and find entries" should {
    "list searchIds" in new TempMongoDBService {
      service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      val searchIdsList = service.listSearchIds.futureValue
      searchIdsList.length mustEqual(2)
    }
    "find" in new TempMongoDBService {
      service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      val basketEntries = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntries.length mustEqual(3)
      basketEntries.find(_.peptideSeq == "SLIWTLLK").get.spectrumId.id.value mustEqual("20150318_Petricevic_7371A.8585.8585.2")
    }
  }

  "create and update" should {
    "update rtZoom" in new TempMongoDBService {
      service.insertOrUpdate(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      val entry5_changed = entry5.copy(rtZoom = RtRange(20, 50))
      service.insertOrUpdate(Seq(entry5_changed)).futureValue must equalTo(1)
      service.countBasketEntries.futureValue must equalTo(5)
      val basketEntries = service.findByProtein("F009998,F009999", AccessionCode("K2C1_HUMAN")).futureValue
      basketEntries.length mustEqual(1)
      basketEntries(0).rtZoom mustEqual(RtRange(20, 50))
    }
  }


  import ch.isbsib.proteomics.mzviz.results.basket.JsonBasketFormats._
  import play.api.libs.json._

  val entryJson = """{"proteinAC":"OSBL8_HUMAN","peptideSeq":"Q{Deamidated}DDSYIEPEPVEPLKETTYTEQ{Deamidated}SHEELGEAGEASQTETVSEENK","startPos":361,"endPos":404,"searchIds":"mascot:F002453,mascot:F002454","spectrumId":{"id":"20150318_Petricevic_7371A.10015.10015.4","runId":"mascot:F002453"},"score":88.5, "localizationScore": 90, "ppmTolerance":10,"rtZoom":{"lowerRt":10,"upperRt":30},"rtSelected":{"lowerRt":10,"upperRt":30},"xicPeaks":[]}"""
  val entryJsonPeaks = """{"proteinAC":"OSBL8_HUMAN","peptideSeq":"Q{Deamidated}DDSYIEPEPVEPLKETTYTEQ{Deamidated}SHEELGEAGEASQTETVSEENK","startPos":361,"endPos":404,"searchIds":"mascot:F0024532,mascot:F0024542","spectrumId":{"id":"20150318_Petricevic_7371A.10015.10015.4","runId":"mascot:F002453"},"score":88.5, "localizationScore": null,"ppmTolerance":10,"rtZoom":{"lowerRt":10,"upperRt":30},"rtSelected":{"lowerRt":10,"upperRt":30},"xicPeaks":[{"searchId":"mascot:F002453","rt":37.95,"intensity":6340000},{"searchId":"mascot:F002454","rt":37.97,"intensity":744000}]}"""
  val entryJsonNullPeak = """{"proteinAC":"OSBL8_HUMAN","peptideSeq":"Q{Deamidated}DDSYIEPEPVEPLKETTYTEQ{Deamidated}SHEELGEAGEASQTETVSEENK","startPos":361,"endPos":404,"searchIds":"mascot:F0024533,mascot:F0024543","spectrumId":{"id":"20150318_Petricevic_7371A.10015.10015.4","runId":"mascot:F002453"},"score":88.5, "localizationScore": 90,"ppmTolerance":10,"rtZoom":{"lowerRt":10,"upperRt":30},"rtSelected":{"lowerRt":10,"upperRt":30},"xicPeaks":[{"searchId":"mascot:F002453","rt":41.55,"intensity":238000},{"searchId":"mascot:F002454","rt":null,"intensity":null}]}"""


  "insert json" should {
    "insert OSBL8_HUMAN no peaks" in new TempMongoDBService {

      val basketEntry:BasketEntry = Json.parse(entryJson).as[BasketEntry]
      service.insertOrUpdate(Seq(basketEntry)).futureValue must equalTo(1)
      service.countBasketEntries.futureValue must equalTo(1)
      val basketEntries = service.findByProtein("mascot:F002453,mascot:F002454", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntries.length mustEqual(1)
      basketEntries(0).xicPeaks.length mustEqual(0)
    }

    "insert OSBL8_HUMAN with peaks" in new TempMongoDBService {

      val basketEntry:BasketEntry = Json.parse(entryJsonPeaks).as[BasketEntry]
      service.insertOrUpdate(Seq(basketEntry)).futureValue must equalTo(1)
      service.countBasketEntries.futureValue must equalTo(1)
      val basketEntries = service.findByProtein("mascot:F0024532,mascot:F0024542", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntries.length mustEqual(1)
      basketEntries(0).xicPeaks.length mustEqual(2)
    }


    "insert OSBL8_HUMAN with null peaks" in new TempMongoDBService {

      val basketEntry:BasketEntry = Json.parse(entryJsonNullPeak).as[BasketEntry]
      service.insertOrUpdate(Seq(basketEntry)).futureValue must equalTo(1)
      service.countBasketEntries.futureValue must equalTo(1)
      val basketEntries = service.findByProtein("mascot:F0024533,mascot:F0024543", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntries.length mustEqual(1)
      basketEntries(0)._id.isEmpty mustEqual(false)
      basketEntries(0).xicPeaks.length mustEqual(2)
    }

  }

}