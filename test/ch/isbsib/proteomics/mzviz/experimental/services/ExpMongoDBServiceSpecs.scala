package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import org.scalatest.time.{Seconds, Span, Millis}
import org.specs2.mutable.{BeforeAfter, Specification}
import org.specs2.specification.After
import play.api.Logger
import reactivemongo.api.{DefaultDB, MongoDriver}
import scala.concurrent._
import org.scalatest.concurrent.ScalaFutures
import ExecutionContext.Implicits.global
import scala.util.Random
import scala.concurrent.duration._

/**
 * @author Roman Mylonas
 */
/*
class ExpMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(2, Seconds), interval = Span(5, Millis))

  def createDB(dbNamePefix: String, host: String = "localhost:27017") = {
    val driver = new MongoDriver
    val connection = driver.connection(List(host))

    val dbName = s"$dbNamePefix-${new Random().nextLong}"
    println(s"creating a mongodb named $dbName")
    new ExpMongoDBService(connection.db(dbName))
  }

  def dropDBService(service: ExpMongoDBService) = {
    service.db.drop()
    println(s"dropped ${service.db.name}")
  }

  trait tmpService extends After {
    lazy val service = createDB("test")

    def after = dropDBService(service)
  }


  "empty service" should {
    "counts are 0" in new tmpService {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)
    }
  }
  "create 2 runs" should {
    "get them up " in new tmpService {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)

      Await.result(service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-1"))), 1000000 microseconds)
      service.countMsnSpectra.futureValue must equalTo(123)
      service.countMsRuns.futureValue must equalTo(1)

      Await.result(service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-2"))), 1000000 microsecond)
      service.countMsnSpectra.futureValue must equalTo(246)
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(IdRun("test-1"), IdRun("test-2")))
    }
  }

}
*/
