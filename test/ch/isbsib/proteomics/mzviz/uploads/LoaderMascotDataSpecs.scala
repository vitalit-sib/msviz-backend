package ch.isbsib.proteomics.mzviz.uploads


import ch.isbsib.proteomics.mzviz.commons.{Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMs1BinMongoDBService, ExpMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.MatchMongoDBService
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import java.io.{IOException, File}

import reactivemongo.api.{MongoDriver, DefaultDB}

import scala.concurrent.Future
import scala.util.Random


/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMascotDataSpecs extends Specification with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val loaderService = new LoaderMascotData(db)

    val exp1Service = new ExpMs1BinMongoDBService(db)
    val exp2Service = new ExpMongoDBService(db)
    val matchService = new MatchMongoDBService(db)
  }

  "load unzipped" should {

    "load unzipped data" in new TempMongoDBService {

      val unzipped = "test/resources/uploads/mascot_test"
      val futureResults: Future[Seq[SearchId]] = loaderService.loadUnzipped(unzipped, 1.0)

      val results = futureResults.futureValue
      results.size mustEqual 2
      results(0).value.replaceAll("\\d", "") mustEqual ("sample")

      // check ms1
      val ms1List = exp1Service.findMs1EntryWithMozTol(RunId("sample1"), Moz(519.14), 0.3).futureValue
      ms1List.size mustEqual (148)

      val ms2List = exp2Service.findAllSpectraRefByrunId(Set(RunId("sample1"), RunId("sample2"))).futureValue
      ms2List.size mustEqual (82)

      val matchList = matchService.findAllSpectrumIdBySearchId(SearchId("sample1")).futureValue
      matchList.size mustEqual (62)

    }

    "load unzipped with resubmitted job" in new TempMongoDBService {

      val unzipped = "test/resources/uploads/mascot_test_renamed"
      val futureRes: Future[Seq[SearchId]] = loaderService.loadUnzipped(unzipped, 1.0)

      val results = futureRes.futureValue
      results.size mustEqual 2

      // check ms1
      val ms1List = exp1Service.findMs1EntryWithMozTol(RunId("sample1"), Moz(519.14), 0.3).futureValue
      ms1List.size mustEqual (148)

      val ms2List = exp2Service.findAllSpectraRefByrunId(Set(RunId("sample1"), RunId("sample2"))).futureValue
      ms2List.size mustEqual (82)

      val matchList = matchService.findAllSpectrumIdBySearchId(SearchId("sample1")).futureValue
      matchList.size mustEqual (62)

    }

  }

  "load zip" should {

    "load zip data" in new TempMongoDBService{

      val zipFile = new File("test/resources/uploads/mascot_test.zip")
      val futureRes: Future[Seq[SearchId]] = loaderService.loadZip(zipFile, 500000)

      val results = futureRes.futureValue
      results.size mustEqual 2

      // check ms1
      val ms1List = exp1Service.findMs1EntryWithMozTol(RunId("sample1"), Moz(519.14), 0.3).futureValue
      ms1List.size mustEqual(15)

      val ms2List = exp2Service.findAllSpectraRefByrunId(Set(RunId("sample1"), RunId("sample2"))).futureValue
      ms2List.size mustEqual(82)

      val matchList = matchService.findAllSpectrumIdBySearchId(SearchId("sample1")).futureValue
      matchList.size mustEqual(62)

    }

    "load zip with no dir" in new TempMongoDBService{

      val zipFile = new File("test/resources/uploads/mascot_nodir.zip")
      val futureRes: Future[Seq[SearchId]] = loaderService.loadZip(zipFile, 500000)

      val results = futureRes.futureValue
      results.size mustEqual 2

      // check ms1
      val ms1List = exp1Service.findMs1EntryWithMozTol(RunId("sample1"), Moz(519.14), 0.3).futureValue
      ms1List.size mustEqual(15)

      val ms2List = exp2Service.findAllSpectraRefByrunId(Set(RunId("sample1"), RunId("sample2"))).futureValue
      ms2List.size mustEqual(82)

      val matchList = matchService.findAllSpectrumIdBySearchId(SearchId("sample1")).futureValue
      matchList.size mustEqual(62)

    }

  }




}
