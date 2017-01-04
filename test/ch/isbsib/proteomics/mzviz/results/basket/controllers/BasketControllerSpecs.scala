package ch.isbsib.proteomics.mzviz.results.basket.controllers

import java.util.Calendar

import ch.isbsib.proteomics.mzviz.commons.services.MongoId
import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.results.basket.BasketMongoDBService
import ch.isbsib.proteomics.mzviz.results.basket.models.{BasketEntry, RtRange, XicPeak}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span}
import play.api.test.FakeRequest
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.specs2.mutable.Specification
import play.api.libs.json._
import play.api.test._
import play.api.test.Helpers._
import ch.isbsib.proteomics.mzviz.results.basket.JsonBasketFormats._

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2017, SIB Swiss Institute of Bioinformatics
 */
class BasketControllerSpecs extends Specification with ScalaFutures{

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  val entry1 = new BasketEntry(None, proteinAC = AccessionCode("OSBL8_HUMAN"),
    peptideSeq = "SLIWTLLK",
    startPos = 405,
    endPos = 412,
    searchIds = "F002453X,F002454X",
    spectrumId = SpectrumId(id = SpectrumUniqueId("20150318_Petricevic_7371A.8585.8585.2"), runId = RunId("F002453")),
    score = 87.5,
    localizationScore = Some(100),
    ppmTolerance = 10.0,
    rtZoom = RtRange(lowerRt = 35, upperRt = 39),
    rtSelected = RtRange(lowerRt = 36, upperRt = 37),
    xicPeaks = Seq(XicPeak(SearchId("F002453"),Some(RetentionTime(36.48)), Some(Intensity(198000))), XicPeak(SearchId("F002453"), Some(RetentionTime(36.55)), Some(Intensity(621000)))),
    creationDate = Some(Calendar.getInstance().getTime()),
    prevAA = Some("A"),
    nextAA = Some("C"),
    ppmDiff = Some(0.56)
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
          basketEntries(0).creationDate.isDefined mustEqual(true)

      }
    }

//    "insert and delete" in {
//      running(FakeApplication()){
//        val fakeRequest = FakeRequest(PUT, "/basket").withTextBody(Json.toJson(entry1).toString())
//        val result = route(fakeRequest).get
//
//        val bodyText: String = contentAsString(result)
//        bodyText must be equalTo "{\"nrInserted\":1}"
//
//
//        val basketEntries = BasketMongoDBService().findByProtein("F002453X,F002454X", AccessionCode("OSBL8_HUMAN")).futureValue(timeout = Timeout(Span(60, Seconds)))
//        basketEntries.length mustEqual(1)
//
//        val fakeRequestDel = FakeRequest(DELETE, "/basket/" + basketEntries(0)._id.get.$oid).withTextBody(Json.toJson(entry1).toString())
//        val resultDel = route(fakeRequestDel).get
//        contentAsString(resultDel) must be equalTo "{\"true\"}"
//
//        val basketEntriesDel = BasketMongoDBService().findByProtein("F002453X,F002454X", AccessionCode("OSBL8_HUMAN")).futureValue(timeout = Timeout(Span(60, Seconds)))
//        basketEntriesDel.length mustEqual(9)
//
//      }
//    }

  }
}