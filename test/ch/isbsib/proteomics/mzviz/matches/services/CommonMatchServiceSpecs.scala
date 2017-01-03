package ch.isbsib.proteomics.mzviz.matches.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class CommonMatchServiceSpecs extends Specification with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new CommonMatchService(db)
    val searchInfoService = new SearchInfoDBService(db)
    val protMatchService = new ProteinMatchMongoDBService(db)
    val matchService = new MatchMongoDBService(db)

  }

  "delete all match info" should {
    val file = new File("test/resources/mascot/M_100.mzid")
    val searchId = SearchId("coucou")
    val mzIdent = scala.xml.XML.loadFile(file)
    val matchData = LoaderMzIdent.parse(file, searchId, RunId("1"), None)

    "counts are 0" in new TempMongoDBService {
      matchService.insert(matchData._1).futureValue
      protMatchService.insert(matchData._2).futureValue
      searchInfoService.insert(matchData._3).futureValue

      matchService.listRunIds.futureValue.size mustEqual(1)
      protMatchService.countEntries.futureValue mustEqual(27)
      searchInfoService.list.futureValue.size mustEqual(1)

      service.deleteAllMatchInfo(searchId)
      matchService.listRunIds.futureValue.size mustEqual(0)
      protMatchService.countEntries.futureValue mustEqual(0)
      searchInfoService.list.futureValue.size mustEqual(0)
    }

  }

}