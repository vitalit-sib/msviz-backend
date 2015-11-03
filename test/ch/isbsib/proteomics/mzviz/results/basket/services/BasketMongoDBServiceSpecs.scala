package ch.isbsib.proteomics.mzviz.results.basket.services

import ch.isbsib.proteomics.mzviz.commons.{RetentionTime, Intensity, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.results.basket.models.{XicPeak, RtRange, BasketEntry}
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

  val entry1 = new BasketEntry(proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "SLIWTLLK",
    startPos = 405,
    endPos = 412,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7371A.8585.8585.2"), runId = RunId("F002453")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 35, upperRt = 39),
    rtSelected = RtRange(lowerRt = 36, upperRt = 37),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),RetentionTime(36.48),Intensity(198000)), XicPeak(SearchId("F002453"),RetentionTime(36.55),Intensity(621000)))
  )

  val entry2 = new BasketEntry(proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "SLIWT{Phospho}LLK",
    startPos = 405,
    endPos = 412,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7371A.12161.12161.2"), runId = RunId("F002453")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 47.5, upperRt = 48.5),
    rtSelected = RtRange(lowerRt = 47.8, upperRt = 48.2),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),RetentionTime(47.93),Intensity(472000)), XicPeak(SearchId("F002453"),RetentionTime(47.94),Intensity(1470000)))
  )

  val entry3 = new BasketEntry(proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "ANNLHSGDN{Deamidated}FQLNDS{Phospho}EIER",
    startPos = 300,
    endPos = 318,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7374A.9189.9189.3"), runId = RunId("F002454")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 36.5, upperRt = 39.5),
    rtSelected = RtRange(lowerRt = 37.5, upperRt = 38.0),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),RetentionTime(37.74),Intensity(139000)), XicPeak(SearchId("F002453"),RetentionTime(37.82),Intensity(634000)))
  )

  val entry4 = new BasketEntry(proteinAC = AccessionCode("K2C1_HUMAN"),
    peptideSeq = "MS{Phospho}GEC{Carbamidomethyl}APN{Deamidated}VSVSVSTSHTTISGGGSR",
    startPos = 493,
    endPos = 518,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7374A.6984.6984.3"), runId = RunId("F002454")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 29.5, upperRt = 31.5),
    rtSelected = RtRange(lowerRt = 30.0, upperRt = 30.5),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),RetentionTime(30.51),Intensity(95400)), XicPeak(SearchId("F002453"),RetentionTime(30.30),Intensity(3620000)))
  )

  val entry5 = new BasketEntry(proteinAC = AccessionCode("K2C1_HUMAN"),
    peptideSeq = "MS{Phospho}GEC{Carbamidomethyl}APN{Deamidated}VSVSVSTSHTTISGGGSR",
    startPos = 493,
    endPos = 518,
    searchIds = "F009998,F009999",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7374A.6984.6984.3"), runId = RunId("F009999")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 29.5, upperRt = 31.5),
    rtSelected = RtRange(lowerRt = 30.0, upperRt = 30.5),
    xicPeaks = Seq(XicPeak(SearchId("F009998"),RetentionTime(30.51),Intensity(95400)), XicPeak(SearchId("F009999"),RetentionTime(30.30),Intensity(3620000)))
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
      service.insert(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      service.countBasketEntries.futureValue must equalTo(5)
      service.listProteins("1,2").futureValue.length must equalTo(0)
      val proteinList = service.listProteins("F002453,F002454").futureValue
      proteinList.length mustEqual(2)
      proteinList(0) must equalTo(AccessionCode("OSBL8_HUMAN"))
    }
  }

  "create and delete" should {
    "create 5 and delete all" in new TempMongoDBService {
      service.insert(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      service.countBasketEntries.futureValue must equalTo(5)
      service.deleteBySearchId("F002453")
      service.countBasketEntries.futureValue must equalTo(1)
    }
  }

  "list and find entries" should {
    "list searchIds" in new TempMongoDBService {
      service.insert(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      val searchIdsList = service.listSearchIds.futureValue
      searchIdsList.length mustEqual(2)
      searchIdsList(0) mustEqual("F002453,F002454")
    }
    "find" in new TempMongoDBService {
      service.insert(Seq(entry1, entry2, entry3, entry4, entry5)).futureValue must equalTo(5)
      val basketEntries = service.findByProtein("F002453,F002454", AccessionCode("OSBL8_HUMAN")).futureValue
      basketEntries.length mustEqual(3)
      basketEntries(0).peptideSeq mustEqual("SLIWTLLK")
    }

  }


}