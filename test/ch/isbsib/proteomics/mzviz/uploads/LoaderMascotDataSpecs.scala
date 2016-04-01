package ch.isbsib.proteomics.mzviz.uploads


import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.MatchMongoDBService
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification
import java.io.{IOException, File}

import reactivemongo.api.{MongoDriver, DefaultDB}

import scala.util.Random


/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMascotDataSpecs extends Specification with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))


  "check required files" in {

    val dir = new File ("test/resources/uploads/sample1")

    val requiredFilesMap = LoaderMascotData().getRequiredFiles(Set("mzid", "mgf", "mzml"), dir)

    requiredFilesMap.get("mzid").get.getName mustEqual ("sample1.mzid")
    requiredFilesMap.get("mgf").get.getName mustEqual ("sample1.mgf")
    requiredFilesMap.get("mzml").get.getName mustEqual ("sample1.mzML")

  }


  "check runId from path" in {

    val dir = new File ("test/resources/uploads/sample1")

    val runId = LoaderMascotData().getRunIdFromPath(dir)

    runId mustEqual("sample1")

  }


}