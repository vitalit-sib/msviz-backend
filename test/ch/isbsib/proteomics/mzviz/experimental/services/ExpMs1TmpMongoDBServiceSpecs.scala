package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.{LoaderMzML, LoaderMzXML}
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMs1Spectrum
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import play.api.libs.json.JsValue
import play.api.test.FakeApplication
import play.api.test.Helpers._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1TmpMongoDBServiceSpecs extends Specification with ScalaFutures{

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  val ms1peakIntensityThreshold = 0.0

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new ExpMs1TmpMongoDBService(db)
  }

//  "insert one ms1 spectrum" should {
//    "return 98 entries " in new TempMongoDBService {
//        val ms1Iter = LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("wewe"))
//        val ms1 = ms1Iter.next
//        val nr = service.insertMs1spectrum(ms1, 10000).futureValue
//
//        nr mustEqual (388)
//    }
//  }

  "insert multiple ms1" should {
    "return 98 entries " in new TempMongoDBService {
      val msIter = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("wewe"))
      val ms1Iter: Iterator[ExpMs1Spectrum] = msIter.filter(_.isLeft).map(_.left.get).slice(0,2)
      val nr = service.insertMs1spectra(ms1Iter, 10000).futureValue

      nr mustEqual (3920)
    }
  }


}
