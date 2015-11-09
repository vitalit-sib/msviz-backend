package ch.isbsib.proteomics.mzviz.results.basket.controllers

import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.results.basket.BasketMongoDBService
import ch.isbsib.proteomics.mzviz.results.basket.models.{XicPeak, RtRange, BasketEntry}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import play.api.test.FakeRequest
import ch.isbsib.proteomics.mzviz.controllers.results.BasketController
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification
import play.api.libs.json._
import play.api.test._
import play.api.test.Helpers._
import ch.isbsib.proteomics.mzviz.results.basket.JsonBasketFormats._

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class BasketControllerSpecs extends Specification with ScalaFutures{

  val entry1 = new BasketEntry(proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "SLIWTLLK",
    startPos = 405,
    endPos = 412,
    searchIds = "F002453X,F002454X",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7371A.8585.8585.2"), runId = RunId("F002453")),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 35, upperRt = 39),
    rtSelected = RtRange(lowerRt = 36, upperRt = 37),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),Some(RetentionTime(36.48)), Some(Intensity(198000))), XicPeak(SearchId("F002453"), Some(RetentionTime(36.55)), Some(Intensity(621000))))
  )

  "basket results" should {
      "insert new entry" in {
        running(FakeApplication()){
          val fakeRequest = FakeRequest(PUT, "/basket").withTextBody(Json.toJson(entry1).toString())
          val result = route(fakeRequest).get

          val bodyText: String = contentAsString(result)
          bodyText must be equalTo "{\"nrInserted\":1}"

          val basketEntries = BasketMongoDBService().findByProtein("F002453X,F002454X", AccessionCode("OSBL8_HUMAN")).futureValue
          basketEntries.length mustEqual(1)

      }
    }

  }
}