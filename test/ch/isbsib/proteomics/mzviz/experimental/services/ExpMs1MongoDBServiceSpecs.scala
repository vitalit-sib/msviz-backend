package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMzXML
import ch.isbsib.proteomics.mzviz.experimental.models.Ms1Entry
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

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
/*
  "insert 2 ms1" should {
    "return 98 entries " in new TempMongoDBService {

      val n=LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe"))
      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe")))

      n.size mustEqual(98)
    }
  }

  "find ms1" should {
    "return 8 entries " in new TempMongoDBService {
      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/tiny1_mzXML.mzxml"), RunId("tiny")))


      Thread.sleep(200)
      service.findMs1ByRunId(RunId("tiny")).futureValue.size mustEqual (8)

    }
  }

  "delete 8 ms1" should {
    "remove 8 " in new TempMongoDBService {

      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/tiny1_mzXML.mzxml"), RunId("tiny")))
      //service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe")))

      Thread.sleep(200)
      service.delete(RunId("tiny"))
      Thread.sleep(200)
      service.findMs1ByRunId(RunId("tiny")).futureValue.size mustEqual(0)
    }
  }


  "find ms1 param" should {
    "with moz and tolerance " in new TempMongoDBService {

      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/tiny1_mzXML.mzxml"), RunId("tiny")))
      Thread.sleep(200)
      service.findMs1ByRunID_MozAndTol(RunId("tiny"), Moz(2.0), 0.002).futureValue.size mustEqual(2)

      Thread.sleep(200)
      val fullList=service.findMs1ByRunID_MozAndTol(RunId("tiny"), Moz(2.0), 0.002).futureValue
      val splitListJson= service.extract2Lists(service.findMs1ByRunID_MozAndTol(RunId("tiny"), Moz(2.0), 0.002)).futureValue
      print (splitListJson)
    }
  }
*/
  "insert small ms1" should {
    "return 2 scan and 8 entries " in new TempMongoDBService {

      val n=LoaderMzXML.parseFile(new File("test/resources/ms1/tiny1_mzXML.mzxml"), RunId("tiny"))
      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/tiny1_mzXML.mzxml"), RunId("tiny")))
        .futureValue mustEqual(8)

      n.size mustEqual(2)
    }
  }


}
