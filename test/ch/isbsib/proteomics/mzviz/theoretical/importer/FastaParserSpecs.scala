package ch.isbsib.proteomics.mzviz.theoretical.importer

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import com.google.common.io.CharSource.CharSequenceCharSource
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

import scala.collection.mutable

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


class FastaParserSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))


  "FastaExtractorACFromHeader.parse()" should {
    ">tr|NC_999_2_1" in {
      FastaExtractorACFromHeader.parse(">tr|NC_999_2_1") must equalTo(AccessionCode("NC_999_2_1"))
    }
    ">sp|P04899|GNAI2_HUMAN Guanine nucleotide-binding protein G(i) subunit alpha-2 OS=Homo sapiens GN=GNAI2 PE=1 SV=3" in {
      FastaExtractorACFromHeader.parse(">sp|P04899|GNAI2_HUMAN Guanine nucleotide-binding protein G(i) subunit alpha-2 OS=Homo sapiens GN=GNAI2 PE=1 SV=3") must equalTo(AccessionCode("P04899"))
    }

  }

  "parse" should {
    val entries = FastaParser("test/resources/M_100small.fasta", SequenceSource("pipo")).parse.toList

    val mapEntries: Map[AccessionCode, FastaEntry] = entries.map(e => (e.proteinRef.AC, e)).toMap

    "check the correct number of entries" in {
      entries must have size 2
    }


    "check ac" in {
      entries.map(_.proteinRef.AC) must equalTo(mutable.ArraySeq(AccessionCode("P04899"), AccessionCode("P07355")))
    }

    "P07355 exists" in {
      mapEntries.get(AccessionCode("P07355")).isDefined must equalTo(true)
    }

    "sequence space must have been removed" in {
      mapEntries(AccessionCode("P07355")).sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }
    "source is None" in {
      mapEntries(AccessionCode("P07355")).proteinRef.source must equalTo(Some(SequenceSource("pipo")))
    }
  }

  "parse with a source" should {
    val entries = FastaParser("test/resources/M_100small.fasta", SequenceSource("manon")).parse

    val mapEntries: Map[AccessionCode, FastaEntry] = entries.map(e => (e.proteinRef.AC, e)).toMap


    "sequence space must have been removed" in {
      mapEntries(AccessionCode("P07355")).sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }
    "source is Manon" in {
      mapEntries(AccessionCode("P07355")).proteinRef.source must equalTo(Some(SequenceSource("manon")))
    }
  }

  "parse trembl" should{
    val entries = FastaParser("test/resources/tr.fasta", SequenceSource("tr")).parse

    "size" in {
      entries must have size 2
    }
  }
}
