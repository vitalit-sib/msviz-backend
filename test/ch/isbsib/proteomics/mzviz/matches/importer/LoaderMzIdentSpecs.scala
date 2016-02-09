package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.{HitRank, SearchId}
import ch.isbsib.proteomics.mzviz.matches.models.{SearchInfo, ProteinIdent, ProteinRef, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.{ProteinIdentifier, AccessionCode, NumDatabaseSequences, SequenceSource}
import org.specs2.mutable.Specification

import play.api.test._
import play.api.test.Helpers._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

class LoaderMzIdentSpecs extends Specification {

  def mzidXml(filename:String) = scala.xml.XML.loadFile(new File(filename))

    "parse spectraFileName" should {

      "parse spectraFileName from M_100" in {
        LoaderMzIdent.parseSpectraFilename(mzidXml("test/resources/mascot/M_100.mzid")) must equalTo("rafts1_123spectra")
      }

      "parse spectraFileName from F001644" in {
        LoaderMzIdent.parseSpectraFilename(mzidXml("test/resources/mascot/F001644.mzid")) must equalTo("20141008_BSA_25cm_column2")
      }

    }

  "parseSearchDbSourceInfo" should {

    "parseSearchDbSourceInfo from M_100" in {
      val dbInfo = LoaderMzIdent.parseSearchDbSourceInfo(mzidXml("test/resources/mascot/M_100.mzid"))
      dbInfo.size must equalTo(1)
      dbInfo(0).id must equalTo("SDB_SwissProt_ID")
      dbInfo(0).version.get must equalTo ("SwissProt_2014_08.fasta")
      dbInfo(0).entries.get must equalTo(546238)

      //dbInfo("SDB_SwissProt_ID") must equalTo(Tuple2(SequenceSource("SwissProt_2014_08.fasta"), NumDatabaseSequences(546238)))
    }

    "parseSearchDbSourceInfo from F001644" in {
      val dbInfo = LoaderMzIdent.parseSearchDbSourceInfo(mzidXml("test/resources/mascot/F001644.mzid"))
      dbInfo.size must equalTo(2)
      //dbInfo("SDB_contaminants_PAF") must equalTo(Tuple2(SequenceSource("contaminants_PAF_20130207_1455.fasta"), NumDatabaseSequences(854)))
      //dbInfo("SDB_custom") must equalTo(Tuple2(SequenceSource("custom_20141007_1128.fasta"), NumDatabaseSequences(854)))
    }

  }

    "parse M_100" should {
      running(FakeApplication()) {
        val psmAndProtLists: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/M_100.mzid"), SearchId("M_100"), RunId("M_100.mgf"))
        val psm = psmAndProtLists._1
        val prots = psmAndProtLists._2


        "check size PSMs" in {
          psm.size must equalTo(62)
        }

        "check size Proteins" in {
          prots.size must equalTo(27)
        }

        "check first peptide" in {
          psm(0).pep.sequence must equalTo("TYTWLK")
          //psm(0).pep.dbSequenceRef must equalTo("hiho")
          psm(0).pep.molMass must equalTo(Some(810.427590116))
        }

        "check first peptide score" in {
          psm(0).matchInfo.score.mainScore must equalTo(31.41)
          psm(0).matchInfo.score.scoreMap("Mascot:expectation value") must equalTo(0.0356686898077671)
        }

        "check peptide match info" in {
          psm(0).matchInfo.isRejected must equalTo(Some(false))
          psm(0).matchInfo.chargeState must equalTo(Some(2))
          // psm(0).matchInfo.massDiff must equalTo(99.99)
          // psm(0).matchInfo.numMissedCleavages must equalTo(999)
          psm(0).matchInfo.rank must equalTo(Some(1))
          // psm(0).matchInfo.totalNumIons must equalTo(999)
        }

        "check first spectrum identifier" in {
          psm(0).spectrumId must equalTo(
            SpectrumId(SpectrumUniqueId(SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 50.227 min, Period: 1, Cycle(s): 2033 (Experiment 4)").value),
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
          psm(29).pep.modificationNames.size must equalTo(psm(29).pep.sequence.length + 2)
          psm(29).pep.modificationNames(8).size must equalTo(1)
          psm(29).pep.modificationNames(8)(0) must equalTo(ModifName("Carbamidomethyl"))
          psm(29).pep.modificationNames(0) must equalTo(Nil)

        }

        "check modifications in pep 29" in {
          psm(30).pep.modificationNames.size must equalTo(psm(30).pep.sequence.length + 2)

          psm(30).pep.modificationNames(0).size must equalTo(1)
          psm(30).pep.modificationNames(0)(0) must equalTo(ModifName("Acetyl"))

          psm(30).pep.modificationNames(8).size must equalTo(2)
          psm(30).pep.modificationNames(8)(0) must (equalTo(ModifName("Cys->ethylaminoAla")) or equalTo(ModifName("Carbamidomethyl")))
          psm(30).pep.modificationNames(8)(0) must (equalTo(ModifName("Cys->ethylaminoAla")) or equalTo(ModifName("Carbamidomethyl")))

          psm(30).pep.modificationNames(10).size must equalTo(1)
          psm(30).pep.modificationNames(10)(0) must equalTo(ModifName("GPIanchor"))

        }

     }

    }

    "parse F001644" should {
      running(FakeApplication()) {
        val psmAndProtLists: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/F001644.mzid"), SearchId("F001644"), RunId("F001644.mgf"))
        val psms = psmAndProtLists._1
        val prots = psmAndProtLists._2

        "check PSMs size" in {
          psms.size must equalTo(437)
        }

        "check Proteins size" in {
          prots.size must equalTo(24)
        }

        "check psm content" in {
          val psmsFlt = psms.filter({ psm =>
            psm.spectrumId.id == SpectrumUniqueId("9985")
          })

          psmsFlt.size must equalTo(1)
        }
      }

    }

  "parse M_100_with_X" should {
    running(FakeApplication()) {
      val psmAndProtLists: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/M_100_with_X.mzid"), SearchId("with_X"), RunId("M_100.mgf"))
      val psm = psmAndProtLists._1

      "check first peptide" in {
        psm(0).pep.sequence must equalTo("TYTXLK")
        psm(0).pep.molMass must equalTo(None)
      }
    }

  }

  "parse modification scores" should {
    running(FakeApplication()) {
      val psmAndProtLists: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/F002687_acetylation.mzid"), SearchId("modif"), RunId("M_100.mgf"))
      val psms = psmAndProtLists._1

      "check modif position score" in {

        val psmsFlt = psms.filter({ psm =>
          psm.spectrumId.id.value == "2329"
        })

        psmsFlt.length mustEqual(2)

        psmsFlt(0).matchInfo.score.scoreMap("Mascot:delta score") mustEqual (97.87)
        psmsFlt(1).matchInfo.score.scoreMap("Mascot:delta score") mustEqual (2.11)
      }

      "check another modif position score" in {
        val psmsFlt = psms.filter({ psm =>
          psm.spectrumId.id == SpectrumUniqueId("2098")
        })

        psmsFlt(0).matchInfo.score.scoreMap("Mascot:delta score") mustEqual (90.46)
        psmsFlt(1).matchInfo.score.scoreMap("Mascot:delta score") mustEqual (9.21)
      }
    }


  }


  "parse protein positions" should {
    running(FakeApplication()) {
      val psmAndProtLists: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/F001303.mzid"), SearchId("modif"), RunId("M_100.mgf"))
      val psms = psmAndProtLists._1

      "check position 1" in {
        val psmsFlt = psms.filter({ psm =>
          psm.spectrumId.id == SpectrumUniqueId("9071")
        })

        psmsFlt.size mustEqual (1)

        psmsFlt(0).proteinList.size mustEqual (3)

        // verify first protein
        psmsFlt(0).proteinList(0).startPos mustEqual (769)
        psmsFlt(0).proteinList(0).endPos mustEqual (773)
        psmsFlt(0).proteinList(0).proteinRef.AC mustEqual (AccessionCode("APAF_MOUSE"))
        psmsFlt(0).proteinList(0).proteinRef.source.get mustEqual (SequenceSource("SwissProt_2013_12.fasta"))

        // verify last protein
        psmsFlt(0).proteinList(2).startPos mustEqual (210)
        psmsFlt(0).proteinList(2).endPos mustEqual (214)
        psmsFlt(0).proteinList(2).proteinRef.AC mustEqual (AccessionCode("GBB2_MOUSE"))
        psmsFlt(0).proteinList(2).proteinRef.source.get mustEqual (SequenceSource("SwissProt_2013_12.fasta"))

      }

      "check position 2" in {
        val psmsFlt = psms.filter({ psm =>
          psm.spectrumId.id == SpectrumUniqueId("10716")
        })

        psmsFlt.size mustEqual (1)

        psmsFlt(0).proteinList.size mustEqual (2)

        // verify first protein
        psmsFlt(0).proteinList(0).startPos mustEqual (180)
        psmsFlt(0).proteinList(0).endPos mustEqual (185)
        psmsFlt(0).proteinList(0).proteinRef.AC mustEqual (AccessionCode("HNRPF_MOUSE"))
        psmsFlt(0).proteinList(0).proteinRef.source.get mustEqual (SequenceSource("SwissProt_2013_12.fasta"))

        // verify last protein
        psmsFlt(0).proteinList(1).startPos mustEqual (82)
        psmsFlt(0).proteinList(1).endPos mustEqual (87)
        psmsFlt(0).proteinList(1).proteinRef.AC mustEqual (AccessionCode("HNRPF_MOUSE"))
        psmsFlt(0).proteinList(1).proteinRef.source.get mustEqual (SequenceSource("SwissProt_2013_12.fasta"))

      }
    }

  }

  "parse passThresholds" should {
    running(FakeApplication()) {
      val psmAndProtLists: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/F001303.mzid"), SearchId("modif"), RunId("M_100.mgf"))
      val psms = psmAndProtLists._1

      "check first" in {
        val psmsFlt = psms.filter({ psm =>
          psm.spectrumId.id == SpectrumUniqueId("11315")
        })

        psmsFlt.size mustEqual (4)

        psmsFlt(0).matchInfo.isRejected mustEqual (Option(false))
        psmsFlt(1).matchInfo.isRejected mustEqual (Option(false))
        psmsFlt(2).matchInfo.isRejected mustEqual (Option(false))
        psmsFlt(3).matchInfo.isRejected mustEqual (Option(true))
      }
    }

  }


  "parse Enzyme title" should {

    val xmlString = "      <Enzymes>\n        <Enzyme id=\"ENZ_0\" cTermGain=\"OH\" nTermGain=\"H\" missedCleavages=\"3\" semiSpecific=\"0\">\n          <SiteRegexp><![CDATA[(?<=[KR])]]></SiteRegexp>\n          <EnzymeName>\n            <cvParam accession=\"MS:1001313\" name=\"Trypsin/P\" cvRef=\"PSI-MS\" />\n          </EnzymeName>\n        </Enzyme>\n      </Enzymes>"
    val xmlEl = scala.xml.XML.loadString(xmlString)

    "check Trypsin/P" in {

      val enzyme = LoaderMzIdent.parseEnzymeFilename(xmlEl)
      enzyme mustEqual("Trypsin/P")

    }

  }


  }
