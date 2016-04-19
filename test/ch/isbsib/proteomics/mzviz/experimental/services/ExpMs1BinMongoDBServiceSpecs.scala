package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.{RunIdAndMozBin, RunId}
import ch.isbsib.proteomics.mzviz.experimental.importer.{LoaderMzML, LoaderMzXML}
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMs1Spectrum
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import play.api.libs.json.JsValue
import play.api.test.Helpers._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1BinMongoDBServiceSpecs extends Specification with ScalaFutures{

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  val ms1peakIntensityThreshold = 0.0

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new ExpMs1BinMongoDBService(db)
  }

  "insert and find ms1" should {

      "findMs1EntryList" in new TempMongoDBService {

          val msIter = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("wewe"))
          val ms1Iter: Iterator[ExpMs1Spectrum] = msIter.filter(_.isLeft).map(_.left.get).slice(0, 2)
          val nr = service.insertMs1spectra(ms1Iter, 10000).futureValue

          val binList = service.findMs1EntryList(Set(RunIdAndMozBin("wewe_400"))).futureValue

          binList.size mustEqual (1)
          binList(0).intList.size mustEqual (17)
          binList(0).mozList(0).value mustEqual (400.9367766415585)

      }

      "findMs1EntryWithMozTol" in new TempMongoDBService {

          val msIter = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("wewe"))
          val ms1Iter: Iterator[ExpMs1Spectrum] = msIter.filter(_.isLeft).map(_.left.get).slice(0, 2)
          val nr = service.insertMs1spectra(ms1Iter, 10000).futureValue

          val entryList = service.findMs1EntryWithMozTol(RunId("wewe"), Moz(400.93), 0.1).futureValue

          entryList.size mustEqual (17)


      }


      "insert and find" in new TempMongoDBService {

          val ms1Iter = LoaderMzXML().parse(new File("test/resources/ms1/F001644_small.mzXML"), RunId("hoho"))
          val nr = service.insertMs1spectra(ms1Iter, 0).futureValue

          val rtTol = 0.5
          val moz = 519.14
          val daltonTolerance = 0.3

          val entryList = service.findMs1EntryWithMozTol(RunId("hoho"), Moz(moz), daltonTolerance)

          val json = service.extract2Lists(entryList, rtTol).futureValue

          val rts = (json \ "rt").as[List[JsValue]]
          rts.length mustEqual (98)
          rts(20).as[Double] mustEqual (5.97779)

          val ints = (json \ "intensities").as[List[JsValue]]
          ints(20).as[Double] mustEqual (5459280.5)

      }


      "insert and delete" in new TempMongoDBService {

        val ms1Iter_1 = LoaderMzXML().parse(new File("test/resources/ms1/F001644_small.mzXML"), RunId("hoho"))
        service.insertMs1spectra(ms1Iter_1, 0).futureValue

        val msIter_2 = LoaderMzXML().parse(new File("test/resources/ms1/F001644_small.mzXML"), RunId("hihi"))
        service.insertMs1spectra(msIter_2, 0).futureValue

        val msIter_3 = LoaderMzXML().parse(new File("test/resources/ms1/F001644_small.mzXML"), RunId("keep"))
        service.insertMs1spectra(msIter_3, 0).futureValue

        val moz = 519.14
        val daltonTolerance = 0.3

        // before deleting "hoho"
        val entryList_1 = service.findMs1EntryWithMozTol(RunId("hoho"), Moz(moz), daltonTolerance).futureValue
        entryList_1.size mustEqual (892)

        // delete "hoho"
        val deleteRes = service.deleteAllByRunIds(Set(RunId("hoho"), RunId("hihi"))).futureValue
        deleteRes mustEqual (1)

        // after deleting "hoho" and "hihi"
        val entryList_2 = service.findMs1EntryWithMozTol(RunId("hoho"), Moz(moz), daltonTolerance).futureValue
        entryList_2.size mustEqual (0)
        val entryList_3 = service.findMs1EntryWithMozTol(RunId("hihi"), Moz(moz), daltonTolerance).futureValue
        entryList_3.size mustEqual (0)

        // "keep" should still be the same
        val entryList_4 = service.findMs1EntryWithMozTol(RunId("keep"), Moz(moz), daltonTolerance).futureValue
        entryList_4.size mustEqual (892)


    }


  }



}
