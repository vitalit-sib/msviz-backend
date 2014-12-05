package ch.isbsib.proteomics.mzviz.matches.importer

import ch.isbsib.proteomics.mzviz.matches.ProteinAC
import ch.isbsib.proteomics.mzviz.matches.models.PepSpectraMatch
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

    "parse M_100" should {
      val psm: Seq[PepSpectraMatch] = LoaderMzIdent.parse("test/resources/M_100.mzid")

      "check size" in {
        psm.size must equalTo(62)
      }

      "check first peptide" in {
        psm(0).pep.sequence must equalTo("TYTWLK")
      }

      "check first peptide score" in {
        psm(0).matchInfo.scoreMap("Mascot:score") must equalTo(31.41)
      }

      "check first spectrum identifier" in {
        psm(0).spId must equalTo(SpectraId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 50.227 min, Period: 1, Cycle(s): 2033 (Experiment 4)"))
        psm(0).spSource must equalTo(SpectraSource("rafts1_123spectra"))
      }

      "check protein size" in {
        psm(0).proteinList.size must equalTo(1)
      }

      "check first protein size" in {
        psm(0).proteinList(0).AC must equalTo(ProteinAC("CD109_HUMAN"))
      }

    }

}
