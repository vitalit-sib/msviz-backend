package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{PepSpectraMatch, ProteinIdent}
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.{EvidenceTableEntry, ProteinGroupsTableEntry}
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import net.sf.ehcache.search.expression.EqualTo
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas & Trinidad Martin
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMaxQuantSpecs extends Specification {

  //parse summary.txt to obtain List(RunId)
  val runIdsWithEmpty:Seq[RunId]=LoaderMaxQuant.getRunIds(new File("test/resources/maxquant/summary.txt"))
  val runIds=runIdsWithEmpty.filter(_.value.nonEmpty)

  "parse protein groups" in {

    val listProteinGroups = LoaderMaxQuant.parseProteinGroupTable(new File("test/resources/maxquant/proteinGroups.txt"), runIds)

    listProteinGroups.size mustEqual (139)

    //Select first row
    val entry:ProteinGroupsTableEntry=listProteinGroups(0)
    val listBestMsMs=List(205)
    val listMajority= List("A2A3R7","P62753")
    val keysNSeqMsCount=List(RunId("F002453"), RunId("F002454"))
    val valuesNSeqList=List(0,1)
    val valuesMsCountList=List(0,1)

    listBestMsMs.zip(entry.bestMsMs).forall { case (x, y) => x == y } mustEqual(true)
    listMajority.zip(entry.majorityProtein).forall { case (x, y) => x == y } mustEqual(true)
    keysNSeqMsCount.zip(entry.uniquePeptides.keys.toList).forall { case (x, y) => x == y } mustEqual(true)
    keysNSeqMsCount.zip(entry.msCount.keys.toList).forall { case (x, y) => x == y } mustEqual(true)
    valuesNSeqList.zip(entry.msCount.values.toList).forall { case (x, y) => x == y } mustEqual(true)
    valuesMsCountList.zip(entry.msCount.values.toList).forall { case (x, y) => x == y } mustEqual(true)

    //total amount of nSequences for F002453
    val nSeqTotal=listProteinGroups.map({
      entry => entry.msCount(RunId("F002454"))
    }).sum

    nSeqTotal mustEqual(185)

  }

  "parse source" in {
    val source= LoaderMaxQuant.parseMaxquantParametersTable(new File("test/resources/maxquant/parameters.txt"))("Fasta file")

    source mustEqual("C:\\MaxQuant\\UniProt-ftp-fasta-10-2014\\HUMAN.fasta")
  }

  "parse msms" in {

    val msmsHash=LoaderMaxQuant.parseMaxquantMsMs(new File("test/resources/maxquant/msms.txt"))

    msmsHash.size mustEqual(366)

    //Select second row id=1
    msmsHash(1).id mustEqual(1)
    msmsHash(1).runId mustEqual(RunId("F002454"))
    msmsHash(1).score mustEqual(4.9041)
  }

  "obtain MsMsScore ByRunIdAndId" in {

    val msmsHash=LoaderMaxQuant.parseMaxquantMsMs(new File("test/resources/maxquant/msms.txt"))
    val listIds=List(1)

    val scoreHash= LoaderMaxQuant.obtainMsMsScoreById(listIds, msmsHash)
    scoreHash(RunId("F002454")) mustEqual(4.9041)

    val listIds2=List(0,1,2)
    val scoreHash2= LoaderMaxQuant.obtainMsMsScoreById(listIds2, msmsHash)
    scoreHash2(RunId("F002454")) mustEqual(83.3601)
    scoreHash2(RunId("F002453")) mustEqual(70.942)

  }


  "load ProteinIdents" in {

    val proteinIdMap:Map[RunId,Seq[ProteinIdent]] = LoaderMaxQuant.loadProtIdent("test/resources/maxquant/",runIds)

    // should have 2 runIds
    proteinIdMap.keys.size mustEqual(2)

    proteinIdMap(RunId("F002454")).size mustEqual(101)
    proteinIdMap(RunId("F002453")).size mustEqual(78)

    val oneProt53 = proteinIdMap(RunId("F002453")).filter(p => p.mainProt.proteinAC.value == "H0Y8T4")
    val oneProt54 = proteinIdMap(RunId("F002454")).filter(p => p.mainProt.proteinAC.value == "H0Y8T4")

    oneProt53.head.mainProt.score.mainScore mustEqual(85.813)
    oneProt54.head.mainProt.score.mainScore mustEqual(286.389)

  }

  "parse evidence table" in {

    val listEvidence = LoaderMaxQuant.parseEvidenceTable(new File("test/resources/maxquant/evidence.txt"))

    listEvidence.size mustEqual (363)

    //Select first row
    val entry:EvidenceTableEntry=listEvidence(0)

    entry.id mustEqual(0)
    entry.sequence mustEqual("AAAPQAWAGPMEEPPQAQAPPR")
    entry.experiment mustEqual("F002454")
    entry.molMass.get mustEqual(2286.08515)
    entry.score mustEqual(78.456)
    entry.missedCleavages.get mustEqual(0)
    entry.massDiff.get mustEqual(0.73369)
    entry.chargeState.get mustEqual(3)
    entry.ac mustEqual("Q16643")
  }

  "parse peptides table" in {
    val mapPeptides = LoaderMaxQuant.parsePeptidesTable(new File("test/resources/maxquant/peptides.txt"))

    mapPeptides.size mustEqual(237)

    //Failing because of entries 269;270;271, removed by the filtering because of decoy entry without start and end position

    //Select second row id=2
    mapPeptides(2).evidenceId  mustEqual(2)
    mapPeptides(2).previousAA.get mustEqual("R")
    mapPeptides(2).nextAA.get mustEqual("Y")
    mapPeptides(2).startPos mustEqual(85)
    mapPeptides(2).endPos mustEqual(94)
    mapPeptides(2).isDecoy.get mustEqual(false)
  }

  "load PepSpectraMatch" in {

    val pepSpectraMap:Map[RunId,Seq[PepSpectraMatch]] = LoaderMaxQuant.loadPepSpectraMatch("test/resources/maxquant/",runIds)

    // should have 2 runIds
    pepSpectraMap.keys.size mustEqual(2)

    pepSpectraMap(RunId("F002454")).size mustEqual(95)
    pepSpectraMap(RunId("F002453")).size mustEqual(88)

    val pep10 = pepSpectraMap(RunId("F002454")).filter(p => p.pep.sequence == "AIFQQPPVGVR")
    val pep11 = pepSpectraMap(RunId("F002453")).filter(p => p.pep.sequence == "AIFQQPPVGVR")

    pep10.head.searchId.value mustEqual("F002454")

    pep10.head.spectrumId.id.value mustEqual("6")
    pep10.head.spectrumId.runId.value mustEqual("F002454")

    pep10.head.pep.molMass.get mustEqual(1210.68224)
    pep10.head.pep.sequence mustEqual("AIFQQPPVGVR")
    pep10.head.pep.modificationNames mustEqual(Vector(Seq(ModifName(""))))

    pep10.head.matchInfo.chargeState.get mustEqual(2)
    pep10.head.matchInfo.isRejected mustEqual(None)
    pep10.head.matchInfo.massDiff.get mustEqual(0.42445)
    pep10.head.matchInfo.numMissedCleavages.get mustEqual(0)
    pep10.head.matchInfo.rank mustEqual(None)
    pep10.head.matchInfo.score.mainScore mustEqual(67.113)
    pep10.head.matchInfo.totalNumIons mustEqual(None)

    pep10.head.proteinList(0).isDecoy.get mustEqual(false)
    pep10.head.proteinList(0).endPos mustEqual(686)
    pep10.head.proteinList(0).nextAA.get mustEqual("K")
    pep10.head.proteinList(0).previousAA.get mustEqual("K")
    pep10.head.proteinList(0).startPos mustEqual(676)
    pep10.head.proteinList(0).proteinRef.AC.value mustEqual("Q7L2E3-3")
    pep10.head.proteinList(0).proteinRef.identifiers mustEqual(Set())
    pep10.head.proteinList(0).proteinRef.source mustEqual(None)

    pep11.head.matchInfo.massDiff.get mustEqual(-0.75583)
    pep11.head.proteinList(0).nextAA.get mustEqual ("K")
  }

  "parse Search Info" in {
    val searchInfoMap= LoaderMaxQuant.parseSearchInfo("test/resources/maxquant/")

    // should have 2 runIds
    searchInfoMap.keys.size mustEqual(2)

    val firstEntry=searchInfoMap(RunId("F002453"))

    firstEntry.title mustEqual("F002453")
    firstEntry.enzyme mustEqual("Trypsin/P")
    firstEntry.fragmentTolerance mustEqual("20 ppm")
    firstEntry.parentTolerance mustEqual("-1")
    firstEntry.searchId.value mustEqual("F002453")
    firstEntry.username mustEqual("user")
    firstEntry.database(0).version mustEqual(None)
    firstEntry.database(0).entries mustEqual(None)
    firstEntry.database(0).id mustEqual("C:\\MaxQuant\\UniProt-ftp-fasta-10-2014\\HUMAN.fasta")
  }

  "parse" in {
    val parseMaxQuant= LoaderMaxQuant.parse("test/resources/maxquant/")

    parseMaxQuant.size mustEqual(2)

    val list1=parseMaxQuant(0)
    val list2=parseMaxQuant(1)

    list1._1(0).pep.sequence mustEqual("AELIVQPELK")
    list2._2(0).mainProt.proteinAC.value mustEqual("A2A3R7")
    list2._1(1).matchInfo.score.mainScore mustEqual(4.9041)

  }
}
