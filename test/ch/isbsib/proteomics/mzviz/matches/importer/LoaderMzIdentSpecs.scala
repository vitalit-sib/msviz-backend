package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{ProteinRef, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, NumDatabaseSequences, SequenceSource}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
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
      val dbInfo = LoaderMzIdent.parseSearchDbSourceInfo(new File("test/resources/M_100.mzid"))
      dbInfo.size must equalTo(1)
      dbInfo("SDB_SwissProt_ID") must equalTo(Tuple2(SequenceSource("SwissProt_2014_08.fasta"), NumDatabaseSequences(546238)))
    }

    "parseSearchDbSourceInfo from F001644" in {
      val dbInfo = LoaderMzIdent.parseSearchDbSourceInfo(new File("test/resources/F001644.mzid"))
      dbInfo.size must equalTo(2)
      dbInfo("SDB_contaminants_PAF") must equalTo(Tuple2(SequenceSource("contaminants_PAF_20130207_1455.fasta"), NumDatabaseSequences(854)))
      dbInfo("SDB_custom") must equalTo(Tuple2(SequenceSource("custom_20141007_1128.fasta"), NumDatabaseSequences(854)))
    }

  }

    "parse M_100" should {
      val psm: Seq[PepSpectraMatch] = LoaderMzIdent.parse(new File("test/resources/M_100.mzid"), SearchId("M_100"), RunId("M_100.mgf"))

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
        psm(0).spectrumId must equalTo(
          SpectrumId(SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 50.227 min, Period: 1, Cycle(s): 2033 (Experiment 4)"),
            RunId("M_100.mgf"))
        )
        psm(0).spectrumId.runId must equalTo(RunId("M_100.mgf"))
      }

      "check protein size" in {
        psm(0).proteinList.size must equalTo(1)
      }

      "check first protein content" in {
        psm(0).proteinList(0).proteinRef must equalTo(ProteinRef(AC = AccessionCode("CD109_HUMAN"), source = Some(SequenceSource("SwissProt_2014_08.fasta"))))
        psm(0).proteinList(0).startPos must equalTo(1013)
        psm(0).proteinList(0).endPos must equalTo(1018)
        psm(0).proteinList(0).previousAA must equalTo(Some("R"))
        psm(0).proteinList(0).nextAA must equalTo(Some("G"))
      }

      "check false decoy hit" in {
        psm(0).proteinList(0).isDecoy must equalTo(Some(false))
        psm.last.proteinList.last.isDecoy must equalTo(Some(true))
      }

      "check modifications in pep 29" in {
        psm(29).pep.modificationRefs.size must equalTo(psm(29).pep.sequence.length + 2)
        psm(29).pep.modificationRefs(8).size must equalTo(1)
        psm(29).pep.modificationRefs(8)(0).name must equalTo(ModifName("Carbamidomethyl"))
        psm(29).pep.modificationRefs(0) must equalTo(Nil)

      }

      "check modifications in pep 29" in {
        psm(30).pep.modificationRefs.size must equalTo(psm(30).pep.sequence.length + 2)

        psm(30).pep.modificationRefs(0).size must equalTo(1)
        psm(30).pep.modificationRefs(0)(0).name must equalTo(ModifName("Acetyl"))

        psm(30).pep.modificationRefs(8).size must equalTo(2)
        psm(30).pep.modificationRefs(8)(0).name must (equalTo(ModifName("Cys->ethylaminoAla")) or equalTo(ModifName("Carbamidomethyl")))
        psm(30).pep.modificationRefs(8)(0).name must (equalTo(ModifName("Cys->ethylaminoAla")) or equalTo(ModifName("Carbamidomethyl")))

        psm(30).pep.modificationRefs(10).size must equalTo(1)
        psm(30).pep.modificationRefs(10)(0).name must equalTo(ModifName("GPIanchor"))

      }

    }

    "parse F001644" should {
      val psm: Seq[PepSpectraMatch] = LoaderMzIdent.parse(new File("test/resources/F001644.mzid"), SearchId("F001644"), RunId("F001644.mgf"))

      "check size" in {
        psm.size must equalTo(437)
      }



    }

}
