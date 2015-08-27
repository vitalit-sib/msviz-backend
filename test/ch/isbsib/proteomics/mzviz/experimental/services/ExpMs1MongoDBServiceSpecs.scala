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

  "insert 2 ms1" should {
    "get them up " in new TempMongoDBService {

      print("new")
      service.insertListMS1(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe")))

      //val n: Int = service.insertListMS12(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe"))).futureValue
      //n mustEqual(100)
    }
  }

  "delete 100 ms1" should {
    "remove 100 " in new TempMongoDBService {

      //service.insertListMS12(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe")))

      //service.delete(RunId("wewe"))
    }
  }

  "find ms1" should {
    "get them up " in new TempMongoDBService {

      //service.insertListMS12(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe")))

      //val n = service.findMs1ByRunId(RunId("wewe")).futureValue
    }
  }

  "find ms1" should {
    "with moz and tolerance " in new TempMongoDBService {

      //service.insertListMS12(LoaderMzXML.parseFile(new File("test/resources/ms1/F001644_small.mzxml"), RunId("wewe")))

      //val list: Seq[Ms1Entry] = service.findMs1ByRunID_MozAndTol(RunId("wewe"), Moz(1.5), 0.002).futureValue
    }
  }
}
