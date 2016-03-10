package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{RetentionTime, Moz, TempMongoDBForSpecs, Intensity}
import ch.isbsib.proteomics.mzviz.experimental.{RunIdAndMozBin, RunId}
import ch.isbsib.proteomics.mzviz.experimental.importer.{LoaderMzML, LoaderMzXML}
import ch.isbsib.proteomics.mzviz.experimental.models.{Ms1Bin, ExpMs1Spectrum}
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
//        val rtList = Seq(RetentionTime(45.45), RetentionTime(34.34))
//        val mozList = Seq(Moz(45.45), Moz(34.34))
//        val intList = Seq(Intensity(45.45), Intensity(34.34))
//        val ms1Bin = Ms1Bin(ref="hiho", rtList=rtList, mozList=mozList, intList=intList)
//        val answer = service.insertMs1Bin(ms1Bin).futureValue
//        answer mustEqual (true)
//    }
//  }

//  "insert multiple ms1" should {
//    "return 98 entries " in new TempMongoDBService {
//      val msIter = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("wewe"))
//      val ms1Iter: Iterator[ExpMs1Spectrum] = msIter.filter(_.isLeft).map(_.left.get).slice(0,2)
//      val nr = service.insertMs1spectra(ms1Iter, 10000).futureValue
//
//      nr mustEqual (3920)
//    }
//  }
//
//
  "insert and find ms1" should {
    "findMs1EntryList" in new TempMongoDBService {

      val msIter = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("wewe"))
      val ms1Iter: Iterator[ExpMs1Spectrum] = msIter.filter(_.isLeft).map(_.left.get).slice(0,2)
      val nr = service.insertMs1spectra(ms1Iter, 10000).futureValue

      val binList = service.findMs1EntryList(Set(RunIdAndMozBin("wewe_400"))).futureValue

      binList.size mustEqual(1)
      binList(0).intList.size mustEqual(17)
      binList(0).mozList(0).value mustEqual(400.9367766415585)

    }

    "findMs1EntryWithMozTol" in new TempMongoDBService {

      val msIter = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("wewe"))
      val ms1Iter: Iterator[ExpMs1Spectrum] = msIter.filter(_.isLeft).map(_.left.get).slice(0,2)
      val nr = service.insertMs1spectra(ms1Iter, 10000).futureValue

      val entryList = service.findMs1EntryWithMozTol(RunId("wewe"), Moz(400.93), 0.1).futureValue

      entryList.size mustEqual(17)

    }


  }



}
