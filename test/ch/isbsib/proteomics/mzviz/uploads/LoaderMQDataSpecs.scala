package ch.isbsib.proteomics.mzviz.uploads

import ch.isbsib.proteomics.mzviz.commons.{Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.SearchId
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

  trait TempMongoDBService extends TempMongoDBForSpecs {
    val loaderService = new LoaderMQData(db)

    val exp1Service = new ExpMs1BinMongoDBService(db)
    val exp2Service = new ExpMongoDBService(db)
    val matchService = new MatchMongoDBService(db)
  }

  "load MQ" should {

      """check size""" in new TempMongoDBService{

        val mqZip = "test/resources/uploads/maxQuant.zip"
        val results: Future[Int] = loaderService.loadZip(mqZip, 1)

        results.futureValue mustEqual 2

        // check ms1
        val ms1List = exp1Service.findMs1EntryWithMozTol(RunId("DMSO"), Moz(1957.76), 0.1).futureValue
        ms1List.size mustEqual(5)

        val ms2List = exp2Service.findAllSpectraRefByrunId(Set(RunId("DMSO"), RunId("Nocodazole"))).futureValue
        ms2List.size mustEqual(82)

        val matchList = matchService.findAllSpectrumIdBySearchId(SearchId("DMSO")).futureValue
        matchList.size mustEqual(652)

    }
  }
}
