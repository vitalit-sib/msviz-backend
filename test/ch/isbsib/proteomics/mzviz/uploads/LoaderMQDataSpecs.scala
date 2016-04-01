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

  "load MQ" should {

      """check size""" in new TempMongoDBForSpecs{

          val mqZip = "test/resources/uploads/maxQuant.zip"
          val results: Future[Int] = new LoaderMQData(db).loadZip(mqZip)

          results.futureValue mustEqual 2690

    }
  }
}
