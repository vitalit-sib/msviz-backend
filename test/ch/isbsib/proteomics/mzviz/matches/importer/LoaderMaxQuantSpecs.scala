package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.ProteinIdent
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.ProteinGroupsTableEntry
import net.sf.ehcache.search.expression.EqualTo
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas & Trinidad Martin
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMaxQuantSpecs extends Specification {

  "parse protein groups" in {

    val listProteinGroups = LoaderMaxQuant.parseProteinGroupTable(new File("test/resources/maxquant/proteinGroups.txt"), List(RunId("F002453"), RunId("F002454")))

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
    val source= LoaderMaxQuant.parseMaxquantSource(new File("test/resources/maxquant/parameters.txt"))

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

    val proteinIdMap:Map[RunId,List[ProteinIdent]] = LoaderMaxQuant.loadProtIdent("test/resources/maxquant/")

    // should have 2 runIds
    proteinIdMap.keys.size mustEqual(2)

    proteinIdMap(RunId("F002454")).size mustEqual(101)
    proteinIdMap(RunId("F002453")).size mustEqual(78)

    val oneProt53 = proteinIdMap(RunId("F002453")).filter(p => p.mainProt.proteinAC.value == "H0Y8T4")
    val oneProt54 = proteinIdMap(RunId("F002454")).filter(p => p.mainProt.proteinAC.value == "H0Y8T4")

    oneProt53.head.mainProt.score.mainScore mustEqual(85.813)
    oneProt54.head.mainProt.score.mainScore mustEqual(286.389)

  }


}
