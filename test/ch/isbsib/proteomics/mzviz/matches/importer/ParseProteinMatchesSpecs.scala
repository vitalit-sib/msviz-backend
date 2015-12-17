package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.{SpectrumIdentifictionItem, RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{ProteinIdent, PepSpectraMatch, ProteinRef}
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

class ParseProteinMatchesSpecs extends Specification {


  "parseSpectrumIdAndTitleRelation" should {

    "check size and title" in {
      val mzIdentML = scala.xml.XML.loadFile(new File("test/resources/mascot/F001644.mzid"))
      val spIdTitleRelation = ParseProteinMatches.parseSpectrumIdAndTitleRelation(mzIdentML \\ "SpectrumIdentificationList")

      spIdTitleRelation.get(SpectrumIdentifictionItem("SII_71_1")) must equalTo(Some(SpectrumUniqueId("20141008_BSA_25cm_column2.9985.9985.2")))

      spIdTitleRelation.size must equalTo(437)
    }

  }


  "convertDbSeqId" should {

    val db1 = SearchDatabase(id = "SDB_contaminants_PAF", version=Some("1.0"), entries=Some(999))
    val db2 = SearchDatabase(id = "SDB_custom", version=Some("2.0"), entries=Some(100))
    val searchDbs = Seq(db1, db2)

    "check convertion 1" in {
      def acAndDb = ParseProteinMatches.convertDbSeqId("DBSeq_1_Q0IIK2", searchDbs).get

      acAndDb._1 must equalTo(AccessionCode("Q0IIK2"))
      acAndDb._2 must equalTo(SequenceSource("SDB_contaminants_PAF"))
    }

    "check convertion 2" in {
      def acAndDb = ParseProteinMatches.convertDbSeqId("DBSeq_2_sp|TRFL_HUMAN|", searchDbs).get

      acAndDb._1 must equalTo(AccessionCode("sp|TRFL_HUMAN|"))
      acAndDb._2 must equalTo(SequenceSource("SDB_custom"))
    }

  }


  "parse protein list" should {

    val db1 = SearchDatabase(id = "SDB_contaminants_PAF", version=Some("1.0"), entries=Some(999))
    val db2 = SearchDatabase(id = "SDB_custom", version=Some("2.0"), entries=Some(100))
    val searchDbs = Seq(db1, db2)

    val mzIdentML = scala.xml.XML.loadFile(new File("test/resources/mascot/F001644.mzid"))
    val spIdTitleRelation = ParseProteinMatches.parseProtList(mzIdentML, SearchId("hoho"), searchDbs)

    "check size and title" in {
      spIdTitleRelation.size must equalTo(24)
    }

    "check main protein hit" in {
      spIdTitleRelation(0).mainProt.proteinAC mustEqual (AccessionCode("P02769"))
      spIdTitleRelation(0).mainProt.nrPsms mustEqual (368)
      spIdTitleRelation(0).mainProt.passThreshold mustEqual (true)
      spIdTitleRelation(0).mainProt.score.mainScore mustEqual (10884.08)
      spIdTitleRelation(0).mainProt.nrSequences mustEqual (32)
    }

    "check subsets" in {
      spIdTitleRelation(0).subsetProts.size mustEqual (2)
      spIdTitleRelation(0).subsetProts(0).proteinAC mustEqual (AccessionCode("P02768-1"))
      spIdTitleRelation(0).subsetProts(1).proteinAC mustEqual (AccessionCode("Q3SZ57"))
    }

    "check proteins with special names" in {
      spIdTitleRelation(7).mainProt.proteinAC mustEqual(AccessionCode("mAbEDAR_1_2"))
      spIdTitleRelation(8).mainProt.proteinAC mustEqual(AccessionCode("sp|OVAL_CHICK|"))
      spIdTitleRelation(11).mainProt.proteinAC mustEqual(AccessionCode("REFSEQ:XP_001252647"))

    }

  }


  "check source information" should {

    val file = scala.xml.XML.loadFile(new File("test/resources/mascot/F001644.mzid"))
    val dbInfo = LoaderMzIdent.parseSearchDbSourceInfo(file)
    val spIdTitleRelation = ParseProteinMatches.parseProtList(file, SearchId("hoho"), dbInfo)

    "check size" in {
      spIdTitleRelation.size must equalTo(24)
    }

    "check source info" in {
      spIdTitleRelation(0).mainProt.source must equalTo(SequenceSource("SDB_contaminants_PAF"))
      spIdTitleRelation(2).mainProt.source must equalTo(SequenceSource("SDB_custom"))
    }

  }


}
