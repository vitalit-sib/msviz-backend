package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.experimental.IdRun
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.execute.AsResult
import org.specs2.mutable.{Around, Specification}
import reactivemongo.api.MongoDriver
import reactivemongo.core.commands.GetLastError

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

/**
 * @author Roman Mylonas
 *
 *         TODO change the write concern on the test database
 */

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
    println(s"dropped ${service.db.name}")
    service.db.drop()
  }

  trait tmpService extends Around {
    val service = createDB("test")

    override def around[T: AsResult](t: => T) = {
      val r = try {
        AsResult.effectively(t)
      } catch {
        case e: Throwable => {
          //preform some logic here
          throw e
        }
      }
      dropDBService(service)
      r
    }
  }

  "empty service" should {
    "counts are 0" in new tmpService {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)
    }
  }
  "create 2 runs" should {
    "get them up " in new tmpService {
      //      "solve the destory db firest" >> pending {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)

      val n = service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-1"))).futureValue

      service.countMsnSpectra.futureValue must equalTo(123)
      service.countMsRuns.futureValue must equalTo(1)

      service.insert(LoaderMGF.load("test/resources/M_100.mgf", Some("test-2"))).futureValue

      service.countMsnSpectra.futureValue must equalTo(246)
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(IdRun("test-1"), IdRun("test-2")))
    }
    //    }
  }

}

