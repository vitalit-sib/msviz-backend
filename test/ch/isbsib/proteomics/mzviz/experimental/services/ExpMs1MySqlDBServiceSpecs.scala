package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime, Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.controllers.experimental.ExperimentalController._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.{FastLoaderMzXML, LoaderMzXML}
import ch.isbsib.proteomics.mzviz.experimental.models.{Ms1EntryWithRef, Ms1Peak}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2._
import play.api.Play
import play.api.libs.json.JsValue
import play.api.test.FakeApplication
import play.api.test.Helpers._
import scala.slick.jdbc.meta._
import play.api.db.slick.Config.driver.simple._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


object AppWithTestDb2 extends FakeApplication(additionalConfiguration =
  Map(
    ("db.default.driver") -> "org.h2.Driver",
    ("db.default.url") -> (
      //        "jdbc:h2:mem:play-test-" + scala.util.Random.nextInt +      // in memory database
      "jdbc:h2:/tmp/play-test-" + scala.util.Random.nextInt +     // file based db that can be accessed using h2-browse in activator
        ";MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1")
  ))



class ExpMs1MySqlDBServiceSpecs extends mutable.Specification{

  // setup table and database
  step(Play.start(AppWithTestDb2))

    val ms1Dao = TableQuery[ExpMs1MySqlDBService]

    def createSchema(implicit session: Session) = {
      if(MTable.getTables("MS").list.isEmpty) {
        ms1Dao.ddl.create
        println("table MS was created")
      }else{
        println("table MS already existed")
      }
    }

  trait _Session extends specification.Scope with mutable.After {
    println("Opening session")
    val tableName = s"test${util.Random.nextInt}"
    implicit val session = Database.forURL(s"jdbc:h2:mem:$tableName", driver = "org.h2.Driver").createSession()
    println(s"...session: $session")

    println("Creating schema")
    createSchema(session)

    def after = {
      println("Closing session")
      session.close()
    }
  }


  def insertData(implicit session: Session, file: File, runId: RunId) = {
    val entries = LoaderMzXML.parseFile(file, runId)

    var nrInserted = 0

    while (entries.hasNext) {
      val current = entries.next()
      val runID = current.spId.runId.value
      val rt = current.retentionTime.value

      current.peaks.foreach {
        peak => val ms1 = Ms1Peak(runID, rt, peak.moz.value, peak.intensity.value)
          ms1Dao.insert(ms1)
          nrInserted += 1
      }
    }

    nrInserted
  }

  def insertDataFast(implicit session: Session, file: File, runId: RunId) = {
    val entries = FastLoaderMzXML.parseFile(file, runId)

    val nrInserted = entries.map({ e =>
      val rt = e.retentionTime.value
      val runId = e.spId.runId.value

      val ms1Peaks = e.peaks.map({ peak =>
        Ms1Peak(runId, rt, peak.moz.value, peak.intensity.value)
      })

      // insert peaks into MySql
      ms1Dao ++= ms1Peaks
      ms1Peaks.length
    }).sum

    nrInserted
  }


    "create tables" should {

        "create schema" in new _Session{
          ms1Dao.length.run mustEqual(0)
        }
    }

  "insert Ms1" should {

    "insert F001644_small" in new _Session{
      val nrInserted = insertData(session, new File ("test/resources/ms1/F001644_small.mzXML"), RunId("hoho"))

      nrInserted mustEqual(31771)
      ms1Dao.length.run mustEqual(31771)
    }

    "insert and find" in new _Session{
      val nrInserted = insertData(session, new File ("test/resources/ms1/F001644_small.mzXML"), RunId("hoho"))

      val rtTol = 0.5
      val moz = 519.14
      val daltonTolerance = 0.3

      val ms1List = ms1Dao.filter(ms => (ms.ref === "hoho")
        && (ms.moz <= moz+daltonTolerance)
        && ms.moz >= moz-daltonTolerance).list.map(m => Ms1EntryWithRef(RunId(m.ref), RetentionTime(m.rt), Intensity(m.int), Moz(m.moz))
      )

      val json = ExpMs1MongoDBService().extract2Lists(ms1List, rtTol)

      val rts = (json \ "rt").as[List[JsValue]]
      rts.length mustEqual(98)
      rts(20).as[Double] mustEqual(5.97779)

      val ints = (json \ "intensities").as[List[JsValue]]
      ints(20).as[Double] mustEqual(5459280.5)
    }

  }

  "insert Ms1 fast" should {

    "insert F001644_small fast" in new _Session{
      val nrInserted = insertDataFast(session, new File ("test/resources/ms1/F001644_small.mzXML"), RunId("hoho"))

      nrInserted mustEqual(31771)
      ms1Dao.length.run mustEqual(31771)
    }

    "insert and find fast" in new _Session{
      val nrInserted = insertDataFast(session, new File ("test/resources/ms1/F001644_small.mzXML"), RunId("hoho"))

      val rtTol = 0.5
      val moz = 519.14
      val daltonTolerance = 0.3

      val ms1List = ms1Dao.filter(ms => (ms.ref === "hoho")
        && (ms.moz <= moz+daltonTolerance)
        && ms.moz >= moz-daltonTolerance).list.map(m => Ms1EntryWithRef(RunId(m.ref), RetentionTime(m.rt), Intensity(m.int), Moz(m.moz))
      )

      val json = ExpMs1MongoDBService().extract2Lists(ms1List, rtTol)

      val rts = (json \ "rt").as[List[JsValue]]
      rts.length mustEqual(98)
      rts(20).as[Double] mustEqual(5.97779)

      val ints = (json \ "intensities").as[List[JsValue]]
      ints(20).as[Double] mustEqual(5459280.5)
    }

  }

  "insert and delete" in new _Session {
    insertData(session, new File ("test/resources/ms1/F001644_small.mzXML"), RunId("hoho"))
    insertData(session, new File ("test/resources/ms1/F001644_small.mzXML"), RunId("hihi"))

    ms1Dao.length.run mustEqual(31771 * 2)

    ms1Dao.filter(ms => (ms.ref === "hoho")).delete

    ms1Dao.length.run mustEqual(31771)

  }

  step(Play.stop())

}
