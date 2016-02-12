package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMzXML
import ch.isbsib.proteomics.mzviz.experimental.models.Ms1Entry
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import org.scalatest.MustMatchers
import play.api.libs.json.JsValue

import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1MongoDBServiceSpecs extends Specification with ScalaFutures{

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new ExpMs1MongoDBService(db)
  }

  "insert 2 ms1" should {
    "return 98 entries " in new TempMongoDBService {

      val n=LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("wewe"))
      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("wewe")))
        .futureValue mustEqual(31771)

      n.size mustEqual(98)
    }
  }

  "find ms1" should {
    "return 8 entries " in new TempMongoDBService {
      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("wewe")))

      Thread.sleep(5000)
      service.findMs1ByRunId(RunId("wewe")).futureValue.size mustEqual (31771)

    }
  }

  "delete 8 ms1" should {
    "remove 8 " in new TempMongoDBService {

      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("wewe")))
      Thread.sleep(10000)
      service.delete(RunId("wewe"))
      Thread.sleep(10000)
      service.findMs1ByRunId(RunId("wewe")).futureValue.size mustEqual(0)
    }
  }


  "find ms1 param real data" should {
    "with moz and tolerance " in new TempMongoDBService {

      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("small")))
      Thread.sleep(4000)
      val ms1List = service.findMs1ByRunID_MozAndTol(RunId("small"), Moz(519.14), 0.5).futureValue
      Thread.sleep(4000)
      ms1List.size mustEqual(892)
    }
  }

  "insert small ms1" should {
    "return 2 scan and 8 entries " in new TempMongoDBService {

      val n=LoaderMzXML.parseFile(new File("test/resources/ms1/tiny1_mzXML.mzXML"), RunId("tiny"))
      val scanList = service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/tiny1_mzXML.mzXML"), RunId("tiny")))
        .futureValue

      scanList mustEqual(8)

      n.size mustEqual(2)
    }
  }

  "extract to list of Json objects" should {
    "extract to lists" in new TempMongoDBService {

      val rtTolerance = 0.5

      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("small")))
      Thread.sleep(3000)
      val ms1List = service.findMs1ByRunID_MozAndTol(RunId("small"), Moz(519.14), 0.3).futureValue

      val json = service.extract2Lists(ms1List, rtTolerance)

      val rts = (json \ "rt").as[List[JsValue]]
      rts(20).as[Double] mustEqual(5.97779)

      val ints = (json \ "intensities").as[List[JsValue]]
      ints(20).as[Double] mustEqual(5459280.5)

    }

    "extract to empty lists" in new TempMongoDBService {

      val rtTolerance = 0.5

      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzXML"), RunId("small")))
      Thread.sleep(3000)
      val ms1List = service.findMs1ByRunID_MozAndTol(RunId("small"), Moz(10000.99), 0.0003).futureValue

      val json = service.extract2Lists(ms1List, rtTolerance)

      val rts = (json \ "rt").as[List[JsValue]]
      rts.length mustEqual(0)

      val ints = (json \ "intensities").as[List[JsValue]]
      ints.length mustEqual(0)

    }

  }


}
