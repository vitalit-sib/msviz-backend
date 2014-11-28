package ch.isbsib.proteomics.mzviz.theoretical.importer

import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 */
class FastaParserSpecs extends Specification {
  "parse" should {
    val entries = FastaParser("test/resources/M_100small.fasta").parse

    val mapEntries: Map[String, FastaEntry] = entries.map(e => (e.ac, e)).toMap

    "check the correct number of entries" in {
      entries must have size (2)
    }

    "P07355 exists" in {
      mapEntries.get("P07355").isDefined must equalTo( true)
    }


    "sequence space must have been removed" in {
      mapEntries("P07355").sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }

  }

}
