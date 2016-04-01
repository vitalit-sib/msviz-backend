package ch.isbsib.proteomics.mzviz.uploads

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.services.MatchMongoDBService
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

import scala.concurrent.Future

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMQDataSpecs extends Specification with ScalaFutures{

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new MatchMongoDBService(db)
    val expService = new ExpMongoDBService(db)
    val expMs1Service = new ExpMs1BinMongoDBService(db)
  }


  "load MQ" should {




      """check size""" in new TempMongoDBService{

        //running(FakeApplication()) {
          val mqZip = "test/resources/uploads/maxQuant.zip"
          val results: Future[Int] = LoaderMQData().loadZip(mqZip)

          results.futureValue mustEqual 66
      //}

    }
  }
}
