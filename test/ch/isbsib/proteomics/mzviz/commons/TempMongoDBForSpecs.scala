package ch.isbsib.proteomics.mzviz.commons

import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import org.specs2.execute.AsResult
import org.specs2.mutable.Around
import play.api.Logger
import reactivemongo.api.{DefaultDB, MongoDriver}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
 * Created by tmartinc on 28/11/14.
 */
class TempMongoDBForSpecs extends Around {
  val db:DefaultDB = createDB("scalatest")

  def createDB(dbNamePefix: String, host: String = "localhost:27017") = {
    val driver = new MongoDriver
    val connection = driver.connection(List(host))

    val dbName = s"$dbNamePefix-${new Random().nextLong}"
    Logger.info(s"creating a mongodb named $dbName")
    connection.db(dbName)
  }

  def dropDB = {
    Logger.info(s"dropped ${db.name}")
    db.drop()
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
    dropDB
    r
  }
}