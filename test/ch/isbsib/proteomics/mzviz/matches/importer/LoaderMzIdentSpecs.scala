package ch.isbsib.proteomics.mzviz.matches.importer

import ch.isbsib.proteomics.mzviz.matches.ProteinAC
import ch.isbsib.proteomics.mzviz.matches.models.PepSpectraMatch
import ch.isbsib.proteomics.mzviz.theoretical.{numDatabaseSequences, SequenceSource}
import org.specs2.mutable.Specification
import ch.isbsib.proteomics.mzviz.commons.{SpectraSource, SpectraId}

/**
 *  @author Alexandre Masselot & Roman Mylonas
 */

class LoaderMzIdentSpecs extends Specification {
    "parse spectraFileName" should {

      "parse spectraFileName from M_100" in {
        LoaderMzIdent.parseSpectraFilename("test/resources/M_100.mzid") must equalTo("rafts1_123spectra")
      }

      "parse spectraFileName from F001644" in {
        LoaderMzIdent.parseSpectraFilename("test/resources/F001644.mzid") must equalTo("20141008_BSA_25cm_column2")
      }

    }

  "parseSearchDbSourceInfo" should {

    "parseSearchDbSourceInfo from M_100" in {
      val dbInfo = LoaderMzIdent.parseSearchDbSourceInfo("test/resources/M_100.mzid")
      dbInfo.size must equalTo(1)
      dbInfo(0) must equalTo(Tuple2(SequenceSource("SwissProt_2014_08.fasta"), numDatabaseSequences(546238)))
    }

    "parseSearchDbSourceInfo from F001644" in {
      val dbInfo = LoaderMzIdent.parseSearchDbSourceInfo("test/resources/F001644.mzid")
      dbInfo.size must equalTo(2)
      dbInfo(0) must equalTo(Tuple2(SequenceSource("contaminants_PAF_20130207_1455.fasta"), numDatabaseSequences(854)))
      dbInfo(1) must equalTo(Tuple2(SequenceSource("custom_20141007_1128.fasta"), numDatabaseSequences(854)))
    }

  }

    "parse M_100" should {
      val psm: Seq[PepSpectraMatch] = LoaderMzIdent.parse("test/resources/M_100.mzid")

      "check size" in {
        psm.size must equalTo(62)
      }

      "check first peptide" in {
        psm(0).pep.sequence must equalTo("TYTWLK")
        //psm(0).pep.dbSequenceRef must equalTo("hiho")
        psm(0).pep.molMass must equalTo(810.427590116)
      }

      "check first peptide score" in {
        psm(0).matchInfo.scoreMap("Mascot:score") must equalTo(31.41)
      }

      "check peptide match info" in {
        psm(0).matchInfo.isRejected must equalTo(Some(false))
        // psm(0).matchInfo.massDiff must equalTo(99.99)
        // psm(0).matchInfo.numMissedCleavages must equalTo(999)
        psm(0).matchInfo.rank must equalTo(1)
        // psm(0).matchInfo.totalNumIons must equalTo(999)
      }

      "check first spectrum identifier" in {
        psm(0).spId must equalTo(SpectraId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 50.227 min, Period: 1, Cycle(s): 2033 (Experiment 4)"))
        psm(0).spSource must equalTo(SpectraSource("rafts1_123spectra"))
      }

      "check protein size" in {
        psm(0).proteinList.size must equalTo(1)
      }

      "check first protein content" in {
        psm(0).proteinList(0).AC must equalTo(ProteinAC("CD109_HUMAN"))
        psm(0).proteinList(0).startPos must equalTo(1013)
        psm(0).proteinList(0).endPos must equalTo(1018)
        psm(0).proteinList(0).previousAA must equalTo("R")
        psm(0).proteinList(0).nextAA must equalTo("G")
      }

    }

}
