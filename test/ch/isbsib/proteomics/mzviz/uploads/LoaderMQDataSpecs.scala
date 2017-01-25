package ch.isbsib.proteomics.mzviz.uploads

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.{Moz, TempMongoDBForSpecs}
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.services.MatchMongoDBService
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

import scala.concurrent.Future

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2017, SIB Swiss Institute of Bioinformatics
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

        val mqZip = new File("test/resources/uploads/maxQuant.zip")
        val results: Future[Seq[SearchId]] = loaderService.loadZip(mqZip, 1)

        results.futureValue mustEqual Seq(SearchId("MXQ_DMSO"), SearchId("MXQ_Nocodazole"))

        // check ms1
        val ms1List = exp1Service.findMs1EntryWithMozTol(RunId("MXQ_DMSO"), Moz(1957.76), 0.1).futureValue
        ms1List.size mustEqual(5)

        val ms2List = exp2Service.findAllSpectraRefByrunId(Set(RunId("MXQ_DMSO"), RunId("MXQ_Nocodazole"))).futureValue
        ms2List.size mustEqual(82)

        val matchList = matchService.findAllSpectrumIdBySearchId(SearchId("MXQ_DMSO")).futureValue
        matchList.size mustEqual(1231)

        // check on spectrum
        val oneMs2 = exp2Service.findSpectrumByRunIdAndScanNumber(RunId("MXQ_DMSO"), SpectrumUniqueId("6434")).futureValue
        oneMs2.ref.precursor.molecularMassSource mustEqual(Some("MaxQuant-m/z"))
        oneMs2.ref.precursor.molecularMass.get.value mustEqual(1542.763792)

    }
  }
}
