package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import org.scalatest.time.{Seconds, Span, Millis}
import org.specs2.mutable.Specification
import play.api.Logger
import reactivemongo.api.{DefaultDB, MongoDriver}
import scala.concurrent._
import org.scalatest.concurrent.ScalaFutures
import ExecutionContext.Implicits.global
import scala.util.Random

/**
 * @author Roman Mylonas
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
    service.db.drop()
    println(s"dropped ${service.db.name}")
  }

  val service = createDB("test")

  "create db" should {
    "countMsnSpectra=0" in {
      val nSpectra = service.countMsnSpectra.futureValue
      nSpectra must equalTo(0)
    }
    " 0 runs" in {
      val n = service.countMsRuns.futureValue
      n must equalTo(0)
    }


  }

  step(dropDBService(service))
}
