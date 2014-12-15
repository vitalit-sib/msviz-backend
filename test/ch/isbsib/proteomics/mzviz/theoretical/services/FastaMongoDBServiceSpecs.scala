package ch.isbsib.proteomics.mzviz.theoretical.services

import ch.isbsib.proteomics.mzviz.commons.TempMongoDBForSpecs
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import ch.isbsib.proteomics.mzviz.theoretical.importer.FastaParser
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService
import org.expasy.mzjava.proteomics.io.ms.ident.pepxml.v117.MsmsPipelineAnalysis.MsmsRunSummary.SearchSummary.SequenceSearchConstraint
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

import scala.collection.mutable
import scala.concurrent.Future

/**
 * Created by tmartinc on 05/12/14.
 */


class FastaMongoDBServiceSpecs extends Specification with ScalaFutures {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(5000, Millis))

  /**
   * extends the temp mongodatabase and add a exp service above it
   */
  trait TempMongoDBService extends TempMongoDBForSpecs {
    val service = new SequenceMongoDBService(db)
  }


  "insert" should {
    "insert  2" in new TempMongoDBService {
      val entries = FastaParser("test/resources/M_100small.fasta").parse;
      val n: Int = service.insert(entries).futureValue
      n must equalTo(2)
    }
  }

  "listSources" should {
    "get 2 fasta" in new TempMongoDBService {

      val f1 = service.insert(FastaParser("test/resources/M_100small.fasta", SequenceSource("small-1")).parse)
      val f2 = service.insert(FastaParser("test/resources/M_100small.fasta", SequenceSource("small-2")).parse)
      Future.sequence(List(f1, f2)).futureValue
      val sources = service.listSources.futureValue.sortBy(_.value) mustEqual (List(SequenceSource("small-1"), SequenceSource("small-2")))
    }
  }

  "countSequences" should {
    "get 2 sequences" in new TempMongoDBService {
      val f1 = service.insert(FastaParser("test/resources/M_100small.fasta", SequenceSource("small-1")).parse)
      val f2 = service.insert(FastaParser("test/resources/M_100small.fasta", SequenceSource("small-2")).parse)
      Future.sequence(List(f1, f2)).futureValue
      val n: Int = service.countSequencesBySource(SequenceSource("small-1")).futureValue
      n must equalTo(2)
    }
  }


}
