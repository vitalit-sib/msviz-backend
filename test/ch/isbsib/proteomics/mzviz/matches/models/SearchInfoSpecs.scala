package ch.isbsib.proteomics.mzviz.matches.models

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.services.SearchInfoDBService
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SearchInfoSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mongodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new SearchInfoDBService(db)
  }


  "parser" should {
    "parse returns title, database, username, enzyme, parent tol and fragment tol" in new TempMongoDBService {
      val itSearchEntries =LoaderMzIdent.parseSearchInfo(new File("test/resources/M_100.mzid"), SearchId("M_100"))
      val searchEntries=itSearchEntries.next()
      print (searchEntries)
      val title = searchEntries.title
      val databaseName = searchEntries.database
      val username= searchEntries.username
      val enzyme= searchEntries.enzyme
      val parentTol= searchEntries.parentTolerance
      val fragmentTol=searchEntries.fragmentTolerance

      title mustEqual("test rafts sample 123 spectra for Roman")
      databaseName mustEqual List(SearchDatabase("SDB_SwissProt_ID","SwissProt_2014_08.fasta",546238))
      username mustEqual("roman")
      enzyme mustEqual("Trypsin")
      parentTol mustEqual("0.3 dalton")
      fragmentTol mustEqual("0.3 dalton")
    }
  }

}
