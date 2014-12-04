package ch.isbsib.proteomics.mzviz.theoretical.importer

import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 */
class FastaParserSpecs extends Specification {
  "parse" should {
    val entries = FastaParser("test/resources/M_100small.fasta").parse

    val mapEntries: Map[AccessionCode, FastaEntry] = entries.map(e => (e.ac, e)).toMap

    "check the correct number of entries" in {
      entries must have size (2)
    }


    "check ac" in {
      entries.map(_.ac) must equalTo(List("P04899","P07355"))
    }

    "P07355 exists" in {
      mapEntries.get(AccessionCode("P07355")).isDefined must equalTo( true)
    }


    "sequence space must have been removed" in {
      mapEntries(AccessionCode("P07355")).sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }

  }

}
