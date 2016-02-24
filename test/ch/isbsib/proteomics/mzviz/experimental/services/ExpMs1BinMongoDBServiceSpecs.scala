package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime, Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.{RunIdAndMozBin, RunId}
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMzML
import ch.isbsib.proteomics.mzviz.experimental.models.{Ms1Entry, Ms1EntryList, ExpMs1Spectrum}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import play.api.libs.json.JsValue

import play.api.test._
import play.api.test.Helpers._

/**
 * @author Roman Mylonas & Trinidad Martin
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

  "insertMS1peaks" should {

      "insertMS1peaks check return" in new TempMongoDBService {
        running(FakeApplication()) {

          val ms1SpList: Iterator[ExpMs1Spectrum] = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("small")).filter(_.isLeft).map(_.left.get)

          val res = service.insertMS1peaks(ms1SpList, 100000)

          // check lowest value
          res._1 mustEqual (395.4523269330307)

          // check highest value
          res._2 mustEqual (1923.7513626013747)

          // check number of inserts
          res._3.futureValue mustEqual(41752)
      }

    }

  }


  "insertMS1bins" should {

    "insertMS1bins and find entries" in new TempMongoDBService {
      running(FakeApplication()) {

        val ms1SpList: Iterator[ExpMs1Spectrum] = LoaderMzML().parse(new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML"), RunId("small")).filter(_.isLeft).map(_.left.get).take(3)

        val res = service.insertMS1peaks(ms1SpList, 1000)

        // check number of inserts
        res._3.futureValue mustEqual(5332)

        val nrInserted = service.createMS1bins(RunId("small"), 400.12, 401.43).futureValue

        nrInserted mustEqual(2)

        val resList = service.findMs1EntryWithMozTol(RunId("small"), Moz(400.94), 0.0001).futureValue
        resList.size mustEqual(92)
        resList(0).rt.value mustEqual(35.211123)

      }

    }

  }


  // some test data
  val entry1 = Ms1Entry(RetentionTime(3.455), Intensity(4000.6), Moz(445.54))
  val entry2 = Ms1Entry(RetentionTime(5.6445), Intensity(8000.12), Moz(445.45))
  val entry3 = Ms1Entry(RetentionTime(8.6445), Intensity(9000.12), Moz(445.90))
  val ms1EntryList1: Ms1EntryList = Ms1EntryList(RunIdAndMozBin("hoho_445"), Seq(entry1, entry2, entry3))
  val entry4 = Ms1Entry(RetentionTime(8.455), Intensity(3000.6), Moz(600.54))
  val entry5 = Ms1Entry(RetentionTime(3.455), Intensity(4000.6), Moz(600.64))
  val ms1EntryList2: Ms1EntryList = Ms1EntryList(RunIdAndMozBin("hoho_600"), Seq(entry4, entry5))
  val entry6 = Ms1Entry(RetentionTime(8.455), Intensity(3000.6), Moz(444.98))
  val entry7 = Ms1Entry(RetentionTime(8.455), Intensity(3000.6), Moz(444.58))
  val ms1EntryList3: Ms1EntryList = Ms1EntryList(RunIdAndMozBin("hoho_444"), Seq(entry6, entry7))


  "insert Ms1EntryList" should {

    "check insertMs1EntryList" in new TempMongoDBService {

      val res = service.insertMs1EntryList(ms1EntryList1)

      res.futureValue mustEqual(true)
    }

    "insert and find ms1Bins" in new TempMongoDBService {

      val resInsert = service.insertMs1EntryList(ms1EntryList1)
      resInsert.futureValue mustEqual(true)

      val resFind = service.findMs1EntryList(Set(RunIdAndMozBin("hoho_445")))
      val resList = resFind.futureValue

      resList.size mustEqual(1)
      resList(0).ref.value mustEqual("hoho_445")
      resList(0).ms1EntryList.size mustEqual(3)
      resList(0).ms1EntryList(0).rt.value mustEqual(3.455)

    }

    "insert and dont find ms1Bins" in new TempMongoDBService {

      val resInsert = service.insertMs1EntryList(ms1EntryList1)
      resInsert.futureValue mustEqual(true)

      val resNotFind = service.findMs1EntryList(Set(RunIdAndMozBin("hoho_800")))
      resNotFind.futureValue.size mustEqual(0)
    }

    "insert and find multiple ms1Bins" in new TempMongoDBService {

      val resInsert = service.insertMs1EntryList(ms1EntryList1)
      resInsert.futureValue mustEqual(true)

      val resInsert2 = service.insertMs1EntryList(ms1EntryList2)
      resInsert2.futureValue mustEqual(true)

      val resFind = service.findMs1EntryList(Set(RunIdAndMozBin("hoho_600"), RunIdAndMozBin("hoho_445")))
      val resList = resFind.futureValue

      resList.size mustEqual(2)

      resList(0).ms1EntryList.size mustEqual(3)
      resList(1).ms1EntryList.size mustEqual(2)

    }

  }


  "find Ms1Entries" should {

    "insert and find Ms1Entries" in new TempMongoDBService {

      val res = service.insertMs1EntryList(ms1EntryList1)
      res.futureValue mustEqual (true)

      val resList = service.findMs1EntryWithMozTol(RunId("hoho"), Moz(445.50), 0.1).futureValue
      resList.size mustEqual(2)

      resList(0).moz.value mustEqual(445.54)
      resList(1).moz.value mustEqual(445.45)

    }

    "insert and dont find Ms1Entries" in new TempMongoDBService {

      val res = service.insertMs1EntryList(ms1EntryList1)
      res.futureValue mustEqual (true)

      val resList = service.findMs1EntryWithMozTol(RunId("hoho"), Moz(800.50), 0.1).futureValue
      resList.size mustEqual(0)

    }

    "insert and find in overlapping bin" in new TempMongoDBService {

      val res = service.insertMs1EntryList(ms1EntryList1)
      val res2 = service.insertMs1EntryList(ms1EntryList3)
      res.futureValue mustEqual (true)
      res2.futureValue mustEqual (true)

      val resList = service.findMs1EntryWithMozTol(RunId("hoho"), Moz(445.20), 0.25).futureValue
      resList.size mustEqual(2)
      val resfil1 = resList.filter(_.moz.value == 444.98)
      resfil1.size mustEqual(1)
      val resfil2 = resList.filter(_.moz.value == 445.45)
      resfil2.size mustEqual(1)

    }


  }


  }
