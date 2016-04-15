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

//  "helper functions" should {
//
//    "check required files" in new TempMongoDBService {
//
//      val dir = new File("test/resources/uploads/sample1")
//
//      val requiredFilesMap = loaderService.getRequiredFiles(Set("mzid", "mgf", "mzml"), dir)
//
//      requiredFilesMap.get("mzid").get.getName mustEqual ("sample1.mzid")
//      requiredFilesMap.get("mgf").get.getName mustEqual ("sample1.mgf")
//      requiredFilesMap.get("mzml").get.getName mustEqual ("sample1.mzML")
//
//    }
//
//    "check runId from path" in new TempMongoDBService {
//
//      val dir = new File("test/resources/uploads/sample1")
//
//      val runId: String = loaderService.getRunIdFromPath(dir).value
//
//      runId mustEqual ("sample1")
//
//    }
//
//  }

  "load data" should {

    "load unzipped" in new TempMongoDBService{

      val unzipped = "test/resources/uploads/mascot_test"
      val results: Future[Int] = loaderService.loadUnzipped(unzipped, 1)

      results.futureValue mustEqual(554)

      // check ms1
      val ms1List = exp1Service.findMs1EntryWithMozTol(RunId("sample1"), Moz(519.14), 0.3).futureValue
      ms1List.size mustEqual(148)

      val ms2List = exp2Service.findAllSpectraRefByrunId(Set(RunId("sample1"), RunId("sample2"))).futureValue
      ms2List.size mustEqual(82)

      val matchList = matchService.findAllSpectrumIdBySearchId(SearchId("sample1")).futureValue
      matchList.size mustEqual(62)

    }

    "load zip" in new TempMongoDBService{

      val zipFile = "test/resources/uploads/mascot_test.zip"
      val results: Future[Int] = loaderService.loadZip(zipFile, 500000)

      results.futureValue mustEqual(554)

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
