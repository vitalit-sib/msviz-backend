package ch.isbsib.proteomics.mzviz.results.basket.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{RetentionTime, Intensity, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId, MSRun}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.Peptide
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.results.basket.models.{XicPeak, RtRange, BasketEntry}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import org.expasy.mzjava.core.ms.PpmTolerance
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

  val pep1 = new Peptide(sequence = "SLIWTLLK", molMass = None, modificationNames = scala.collection.immutable.Vector.empty)

  val entry1 = new BasketEntry(proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptide = pep1,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("8585"), runId = RunId("F002453")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 35, upperRt = 39),
    rtSelected = RtRange(lowerRt = 36, upperRt = 37),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),RetentionTime(36.48),Intensity(198000)), XicPeak(SearchId("F002453"),RetentionTime(36.55),Intensity(621000)))
  )

  val modifs2 = Vector(Seq(), Seq(), Seq(), Seq(), Seq(), Seq(ModifName("Phospho")), Seq(), Seq(), Seq(), Seq())

  val pep2 = new Peptide(sequence = "SLIWTLLK", molMass = None, modificationNames = modifs2)

  val entry2 = new BasketEntry(proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptide = pep2,
    searchIds = "F002453,F002454",
    spectrumId = SpectrumId(id = SpectrumUniqueId("12161"), runId = RunId("F002453")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 47.5, upperRt = 48.5),
    rtSelected = RtRange(lowerRt = 47.8, upperRt = 48.2),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),RetentionTime(47.93),Intensity(472000)), XicPeak(SearchId("F002453"),RetentionTime(47.94),Intensity(1470000)))
  )

  "empty service" should {
    "counts are 0" in new TempMongoDBService {
      service.countBasketEntries.futureValue must equalTo(0)
      service.listProteins("1,2").futureValue.length must equalTo(0)
    }
  }

  "create 2 basket entries" should {
    "get them up " in new TempMongoDBService {
      service.insert(Seq(entry1, entry2)).futureValue must equalTo(2)
      service.countBasketEntries.futureValue must equalTo(2)
      service.listProteins("1,2").futureValue.length must equalTo(0)
      val proteinList = service.listProteins("F002453,F002454").futureValue
      proteinList(0) must equalTo(AccessionCode("OSBL8_HUMAN"))

    }
  }

}