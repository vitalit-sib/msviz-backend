package ch.isbsib.proteomics.mzviz.theoretical.importer.services

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.experimental.IdRun
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import ch.isbsib.proteomics.mzviz.theoretical.importer.FastaParser
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

import scala.collection.mutable

/**
 * Created by tmartinc on 05/12/14.
 */



class FastaMongoDBServiceSpecs extends Specification with ScalaFutures{
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mngodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new SequenceMongoDBService(db)
  }


  "parse" should {
    val entries = FastaParser("test/resources/M_100small.fasta").parse

    val mapEntries: Map[AccessionCode, FastaEntry] = entries.map(e => (e.ac, e)).toMap

    "check the correct number of entries" in {
      entries must have size (2)
    }


    "check ac" in {
      entries.map(_.ac) must equalTo(mutable.ArraySeq(AccessionCode("P04899"),AccessionCode("P07355")))
    }

    "P07355 exists" in {
      mapEntries.get(AccessionCode("P07355")).isDefined must equalTo( true)
    }


    "sequence space must have been removed" in {
      mapEntries(AccessionCode("P07355")).sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }

  }

  "insert" should {
    "insert  2" in new TempMongoDBService {
      val entries = FastaParser("test/resources/M_100small.fasta").parse;
      val n:Int = service.insert(entries).futureValue
      n must equalTo(2)
    }
  }


}
