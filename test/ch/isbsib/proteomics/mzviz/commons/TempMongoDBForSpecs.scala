package ch.isbsib.proteomics.mzviz.commons

import org.specs2.execute.AsResult
import org.specs2.mutable.Around
import reactivemongo.api.{DefaultDB, MongoDriver}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
trait TempMongoDBForSpecs extends Around {
  val db: DefaultDB = createDB("scalatest")

  def createDB(dbNamePefix: String, host: String = "localhost:27017") = {
    val driver = new MongoDriver
    val connection = driver.connection(List(host))

    val dbName = s"$dbNamePefix-${new Random().nextLong}"
    //println(s"creating a mongodb named $dbName")
    connection.db(dbName)
  }

  def dropDB = {
    //println(s"dropping ${db.name}")
    //db.drop()
  }

  def around[T: AsResult](t: => T) = {
    val r = try {
      AsResult.effectively(t)
    } catch {
      case e: Throwable =>
        //preform some logic here
        dropDB
        throw e
    }
    dropDB
    r
  }
}