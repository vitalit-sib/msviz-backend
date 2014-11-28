package ch.isbsib.proteomics.mzviz.commons

import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import org.specs2.execute.AsResult
import org.specs2.mutable.Around
import reactivemongo.api.MongoDriver

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
 * Created by tmartinc on 28/11/14.
 */
class TempMongoDBServiceForSpecs extends Around {
  val service = createDB("test")

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