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
  val runIdsWithEmpty:Seq[(RunId, String)]=LoaderMaxQuant.getRunIds(new File("test/resources/maxquant/summary.txt"))
  val runIdsAndRawfiles=runIdsWithEmpty.filter(_._1.value.nonEmpty)
  val runIds = runIdsAndRawfiles.map(_._1)
  val rawfilesRunIdMap: Map[String, RunId] = runIdsAndRawfiles.map(t => Tuple2(t._2, t._1)).toMap

  "parse protein groups" in {

    val listProteinGroups = LoaderMaxQuant.parseProteinGroupTable(new File("test/resources/maxquant/proteinGroups.txt"), runIds)

    listProteinGroups.size mustEqual (165)

    //Select first row
    val entry:ProteinGroupsTableEntry=listProteinGroups(1)

    entry.bestMsMs mustEqual(List(220, 221, 466, 467, 2415))
    entry.majorityProtein mustEqual(List("Q99613","B5ME19"))
    entry.uniquePeptides.keys.toList mustEqual(List(RunId("1-DMSO"), RunId("4-Nocodazole")))
    entry.msCount.keys.toList mustEqual(List(RunId("1-DMSO"), RunId("4-Nocodazole")))
    entry.msCount.values.toList mustEqual(List(3,1))

    //total amount of nSequences for F002453
    val nSeqTotal=listProteinGroups.map({
      entry => entry.msCount(RunId("4-Nocodazole"))
    }).sum

    nSeqTotal mustEqual(1349)

  }

  "parse source" in {
    val source= LoaderMaxQuant.parseMaxquantParametersTable(new File("test/resources/maxquant/parameters.txt"))("Fasta file")

    source mustEqual("C:\\MaxQuant 1.5.3.30\\UniProt-fasta\\Homo_sapiens_091215\\UP000005640_9606.fasta")
  }

  "parse msms" in {

    val msmsHash=LoaderMaxQuant.parseMaxquantMsMs(new File("test/resources/maxquant/msms.txt"), rawfilesRunIdMap)

    msmsHash.size mustEqual(2885)

    //Select second row id=1
    msmsHash(1).id mustEqual(1)
    msmsHash(1).runId mustEqual(RunId("4-Nocodazole"))
    msmsHash(1).score mustEqual(78.456)
  }

  "obtain MsMsScore ByRunIdAndId" in {

    val msmsHash=LoaderMaxQuant.parseMaxquantMsMs(new File("test/resources/maxquant/msms.txt"), rawfilesRunIdMap)
    val listIds=List(1)

    val scoreHash= LoaderMaxQuant.obtainMsMsScoreById(listIds, msmsHash)
    scoreHash(RunId("4-Nocodazole")) mustEqual(78.456)

    val listIds2=List(0,1,2, 3, 4, 5, 6)
    val scoreHash2= LoaderMaxQuant.obtainMsMsScoreById(listIds2, msmsHash)
    scoreHash2.size mustEqual(2)
    scoreHash2(RunId("4-Nocodazole")) mustEqual(457.614)
    scoreHash2(RunId("1-DMSO")) mustEqual(245.61)

  }


  "load ProteinIdents" in {

    val proteinIdMap:Map[RunId,Seq[ProteinIdent]] = LoaderMaxQuant.loadProtIdent("test/resources/maxquant/", runIdsAndRawfiles)

    // should have 2 runIds
    proteinIdMap.keys.size mustEqual(2)

    proteinIdMap(RunId("4-Nocodazole")).size mustEqual(136)
    proteinIdMap(RunId("1-DMSO")).size mustEqual(121)

    val oneProt53 = proteinIdMap(RunId("1-DMSO")).filter(p => p.mainProt.proteinAC.value == "Q99613")
    val oneProt54 = proteinIdMap(RunId("4-Nocodazole")).filter(p => p.mainProt.proteinAC.value == "Q99613")

    oneProt53.head.mainProt.score.mainScore mustEqual(246.776)
    oneProt54.head.mainProt.score.mainScore mustEqual(114.41499999999999)

  }

  "parse evidence table" in {

    val listEvidence = LoaderMaxQuant.parseEvidenceTable(new File("test/resources/maxquant/evidence.txt"))

    listEvidence.size mustEqual (2671)

    //Select first row
    val entry:EvidenceTableEntry=listEvidence(0)

    entry.id mustEqual(0)
    entry.sequence mustEqual("AAAAAEQQQFYLLLGNLLSPDNVVR")
    entry.experiment mustEqual("4-Nocodazole")
    entry.molMass.get mustEqual(2742.43408)
    entry.score mustEqual(159.98)
    entry.missedCleavages.get mustEqual(0)
    entry.massDiff.get mustEqual(0.29854)
    entry.chargeState.get mustEqual(3)
    entry.ac mustEqual("O00410")
  }

  "parse peptides table" in {
    val mapPeptides = LoaderMaxQuant.parsePeptidesTable(new File("test/resources/maxquant/peptides.txt"))

    mapPeptides.size mustEqual(1300)

    //Failing because of entries 269;270;271, removed by the filtering because of decoy entry without start and end position

    //Select second row id=2
    mapPeptides(2).evidenceId  mustEqual(2)
    mapPeptides(2).previousAA.get mustEqual("R")
    mapPeptides(2).nextAA.get mustEqual("N")
    mapPeptides(2).startPos mustEqual(660)
    mapPeptides(2).endPos mustEqual(676)
    mapPeptides(2).isDecoy.get mustEqual(false)
  }

  "load PepSpectraMatch" in {

    val pepSpectraMap:Map[RunId,Seq[PepSpectraMatch]] = LoaderMaxQuant.loadPepSpectraMatch("test/resources/maxquant/",runIds)

    // should have 2 runIds
    pepSpectraMap.keys.size mustEqual(2)

    pepSpectraMap(RunId("4-Nocodazole")).size mustEqual(648)
    pepSpectraMap(RunId("1-DMSO")).size mustEqual(652)

    val pep10 = pepSpectraMap(RunId("4-Nocodazole")).filter(p => p.pep.sequence == "AIFQQPPVGVR")
    val pep11 = pepSpectraMap(RunId("1-DMSO")).filter(p => p.pep.sequence == "AIFQQPPVGVR")

    pep10.head.searchId.value mustEqual("4-Nocodazole")

    pep10.head.spectrumId.id.value mustEqual("40")
    pep10.head.spectrumId.runId.value mustEqual("4-Nocodazole")

    pep10.head.pep.molMass.get mustEqual(1210.68224)
    pep10.head.pep.sequence mustEqual("AIFQQPPVGVR")
    pep10.head.pep.modificationNames mustEqual(Vector(Seq(ModifName(""))))

    pep10.head.matchInfo.chargeState.get mustEqual(2)
    pep10.head.matchInfo.isRejected mustEqual(None)
    pep10.head.matchInfo.massDiff.get mustEqual(0.13029)
    pep10.head.matchInfo.numMissedCleavages.get mustEqual(0)
    pep10.head.matchInfo.rank mustEqual(None)
    pep10.head.matchInfo.score.mainScore mustEqual(67.113)
    pep10.head.matchInfo.totalNumIons mustEqual(None)

    pep10.head.proteinList(0).isDecoy.get mustEqual(false)
    pep10.head.proteinList(0).endPos mustEqual(725)
    pep10.head.proteinList(0).nextAA.get mustEqual("K")
    pep10.head.proteinList(0).previousAA.get mustEqual("K")
    pep10.head.proteinList(0).startPos mustEqual(715)
    pep10.head.proteinList(0).proteinRef.AC.value mustEqual("Q7L2E3")
    pep10.head.proteinList(0).proteinRef.identifiers mustEqual(Set())
    pep10.head.proteinList(0).proteinRef.source mustEqual(None)

    pep11.head.matchInfo.massDiff.get mustEqual(-0.40905)
    pep11.head.proteinList(0).nextAA.get mustEqual ("K")
  }

  "parse Search Info" in {
    val searchInfoMap= LoaderMaxQuant.parseSearchInfo("test/resources/maxquant/")

    // should have 2 runIds
    searchInfoMap.keys.size mustEqual(2)

    val firstEntry=searchInfoMap(RunId("1-DMSO"))

    firstEntry.title mustEqual("1-DMSO")
    firstEntry.enzyme mustEqual("Trypsin/P")
    firstEntry.fragmentTolerance mustEqual("20 ppm")
    firstEntry.parentTolerance mustEqual("-1")
    firstEntry.searchId.value mustEqual("1-DMSO")
    firstEntry.username mustEqual("user")
    firstEntry.database(0).version mustEqual(None)
    firstEntry.database(0).entries mustEqual(None)
    firstEntry.database(0).id mustEqual("C:\\MaxQuant 1.5.3.30\\UniProt-fasta\\Homo_sapiens_091215\\UP000005640_9606.fasta")
  }

  "parse" in {
    val parseMaxQuant= LoaderMaxQuant.parse("test/resources/maxquant/")

    parseMaxQuant.size mustEqual(2)

    val list1=parseMaxQuant(0)
    val list2=parseMaxQuant(1)

    list1._1(0).pep.sequence mustEqual("AAQAPTPGLLQSPR")
    list2._2(0).mainProt.proteinAC.value mustEqual("A0A0C4DH35")
    list2._1(1).matchInfo.score.mainScore mustEqual(78.456)

  }
}
