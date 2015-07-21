package ch.isbsib.proteomics.mzviz.theoretical.models

import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics

 */

class FastaEntrySpecs extends Specification with ScalaFutures {

    "fastaEntry" should {

      // values used in following tests


      "create simple fasta entry" in {
        1 mustEqual 1
      }
    }
  }
