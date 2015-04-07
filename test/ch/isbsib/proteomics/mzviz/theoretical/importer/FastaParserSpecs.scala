package ch.isbsib.proteomics.mzviz.theoretical.importer

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode, ProteinIdentifier}
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
    def check(header:String, ac:String, ids:String) = {
      s"$header -> AC($ac)" in {
        FastaExtractorACFromHeader.parseAC(header) must equalTo(AccessionCode(ac))
      }
      s"$header -> identifiers($ac,$ids)" in {
        val rids = FastaExtractorACFromHeader.parseIdentifiers(header)
        if(ids == ""){
          rids must equalTo (Set())
        }else{
          rids must equalTo((ids.split(",").toList).map(ProteinIdentifier.apply).toSet)
        }
      }
    }

    check(">tr|NC_999_2_1","NC_999_2_1", "")
    check("sp|P04899|GNAI2_HUMAN Guanine nucleotide-binding protein G(i) subunit alpha-2 OS=Homo sapiens GN=GNAI2 PE=1 SV=3", "P04899", "GNAI2_HUMAN")
    check(">P01044-1 SWISS-PROT:P01044-1", "P01044-1", "")
    check(">ENSEMBL:ENSBTAP00000024466 (Bos taurus) 44 kDa protein", "ENSEMBL:ENSBTAP00000024466", "")
    check(">P21578 SWISS-PROT:P21578|LUXY_VIBFI Yellow fluorescent protein (YFP)- Vibrio fischeri.", "P21578", "")
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
    "P07355 identifiers" in {
      mapEntries.get(AccessionCode("P07355")).get.proteinRef.identifiers must equalTo(Set(ProteinIdentifier("P07355"), ProteinIdentifier("ANXA2_HUMAN")))
    }


      "sequence space must have been removed" in {
      mapEntries(AccessionCode("P07355")).sequence must equalTo("MSTVHEILCKLSLEGDHSTPPSAYGSVKAYTNFDAERDALNIETAIKTKGVDEVTIVNILTNRSNAQRQDIAFAYQRRTKKELASALKSALSGHLETVILGLLKTPAQYDASELKASMKGLGTDEDSLIEIICSRTNQELQEINRVYKEMYKTDLEKDIISDTSGDFRKLMVALAKGRRAEDGSVIDYELIDQDARDLYDAGVKRKGTDVPKWISIMTERSVPHLQKVFDRYKSYSPYDMLESIRKEVKGDLENAFLNLVQCIQNKPLYFADRLYDSMKGKGTRDKVLIRIMVSRSEVDMLKIRSEFKRKYGKSLYYYIQQDTKGDYQKALLYLCGGDD")
    }
    "source is pipo" in {
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
