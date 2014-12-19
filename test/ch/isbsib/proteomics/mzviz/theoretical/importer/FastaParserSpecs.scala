package ch.isbsib.proteomics.mzviz.theoretical.importer

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

import scala.collection.mutable

/**
 * Created by tmartinc on 05/12/14.
 */


class FastaParserSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))


  "parse" should {
    val entries = FastaParser("test/resources/M_100small.fasta").parse.toList

    val mapEntries: Map[AccessionCode, FastaEntry] = entries.map(e => (e.ac, e)).toMap

    "check the correct number of entries" in {
      entries must have size (2)
    }


    "check ac" in {
      entries.map(_.ac) must equalTo(mutable.ArraySeq(AccessionCode("P04899"), AccessionCode("P07355")))
    }

    "P07355 exists" in {
      mapEntries.get(AccessionCode("P07355")).isDefined must equalTo(true)
    }

    "sequence space must have been removed" in {
      mapEntries(AccessionCode("P07355")).sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }
    "source is None" in {
      mapEntries(AccessionCode("P07355")).source must equalTo(None)
    }
  }

  "parse with a source" should {
    val entries = FastaParser("test/resources/M_100small.fasta", SequenceSource("manon")).parse

    val mapEntries: Map[AccessionCode, FastaEntry] = entries.map(e => (e.ac, e)).toMap


    "sequence space must have been removed" in {
      mapEntries(AccessionCode("P07355")).sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }
    "source is Manon" in {
      mapEntries(AccessionCode("P07355")).source must equalTo(Some(SequenceSource("manon")))
    }
  }

}
