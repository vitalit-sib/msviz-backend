package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.{MsMsTableEntry, ProteinGroupsTableEntry}
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}

import scala.io.Source._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object LoaderMaxQuant {

  val file_prot = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/proteinGroups.txt")
  val file_params = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/parameters.txt")
  val file_msms = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/msms.txt")
  val file_summary = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/summary.txt")


  def parseCommonLines(file:File): Tuple2[List[List[String]],Map[String, Int]] ={
    val lines: Iterator[String] = fromFile(file).getLines()
    val header = lines.take(1).next.split("\t").toList
    val range = 0 until header.length
    val headerMap: Map[String, Int] = (header zip range).toMap
    //val mProteinsList: List[List[String]] = linesProt.toList.map(s => (s.split("\t")).toList)
    Tuple2(lines.toList.map(s => (s.split("\t")).toList),headerMap)
  }

  def parseProteinGroupTable(file: File, runIds: List[RunId]): List[ProteinGroupsTableEntry] = {

    //From proteinGroups.txt
    //Obtein position for unique peptides, best MS/MS,Majority proteinIDs and MS Count for runiD
    val mProteinsList=parseCommonLines(file)._1
    val headerProtMap=parseCommonLines(file)._2
    val msmsPos: Int = headerProtMap("Best MS/MS")
    val mainProts: Int = headerProtMap("Majority protein IDs")

    val countPosHash:Map[RunId,Int]=runIds.map(runId=>Tuple2(runId, headerProtMap("MS/MS Count " + runId.value))).toMap
    val uniquePepPosHash=runIds.map(runId=>Tuple2(runId, headerProtMap("Unique peptides " + runId.value))).toMap

    mProteinsList.map({
      m => {
        //Obtain ACs
        val acsList: List[String] = m(mainProts).split(";").toList
        //As score we first select the best MS/MS and then we go for the msmsScans.txt to extract the corresponding score
        val bestMsMsFiles = m(msmsPos).split(";").map(_.toInt).toList

        //msCount for runId
        val msCountFiles: Map[RunId,Int] = countPosHash.mapValues(pos=> (m(pos)).toInt)
        //Unique peptides
        val nSeq:Map[RunId,Int] = uniquePepPosHash.mapValues(pos=> (m(pos)).toInt)

        ProteinGroupsTableEntry(bestMsMsFiles, nSeq, acsList, msCountFiles)

      }
    })
  }

  def parseMaxquantSource(file: File) : String ={

    val linesParam: Iterator[String] = fromFile(file).getLines()
    val headerParam = linesParam.take(1).next.split("\t").toList
    val ParamMap: Map[String,String]= linesParam.toList.map(s => Tuple2(s.split("\t")(0),s.split("\t")(1))).toMap
    //val source= ParamMap("Fasta file")
    ParamMap("Fasta file")
  }

  def parseMaxquantMsMs(file:File) : Map[Int,MsMsTableEntry] ={
    //List[MsMsTableEntry]

    //From msms.txt

    val headerScansMap= parseCommonLines(file)._2
    val mScansList:List[List[String]]  = parseCommonLines(file)._1
    val msmsIdPos: Int= headerScansMap("id")
    val scorePos: Int= headerScansMap("Score")
    val rawFilePos:Int= headerScansMap("Raw file")

    //map from id -> info
    val msmsMap=mScansList.map({
      row=>
        val msmsEntry=MsMsTableEntry(RunId(row(rawFilePos)),row(msmsIdPos).toInt, row(scorePos).toDouble)
        Tuple2(row(msmsIdPos).toInt,msmsEntry)
    }).toMap

    msmsMap
  }

  def obtainMsMsScoreById(listIds:List[Int], msmsHash:Map[Int,MsMsTableEntry]):Map[RunId,Double]={


    val filterByIdMsMsEntryList=msmsHash.filterKeys(listIds.toSet).values.toList
    val filterByIdMsMsEntryMapGrouped=filterByIdMsMsEntryList.groupBy(_.runId)
    val msmsEntryScores:Map[RunId,Double]= filterByIdMsMsEntryMapGrouped.mapValues(list => list.map(_.score).sum)

    //return map of RunId and sum of Scores
    msmsEntryScores
  }

  def loadProtMaxQuant(file: File, searchId: SearchId): Map[RunId,List[ProteinIdent]] = {

    //TOCHANGE
    //Seq[Seq[ProteinIdent]]

    //From parameters.txt
    val source=parseMaxquantSource(file_params)

    //From proteinGroups.txt
    val linesSummary: Iterator[String] = fromFile(file_summary).getLines()
    val headerSummary = linesSummary.take(1).next.split("\t").toList
    val rangeSummary= 0 until headerSummary.length
    val headerSummaryMap: Map[String, Int]= (headerSummary zip rangeSummary).toMap
    val mSummaryList:List[List[String]]  = linesSummary.toList.map(s => (s.split("\t")).toList)
    //Obtain list of runIDs
    val runIdsList1: List[RunId] = mSummaryList.map(row=>RunId(row(headerSummaryMap("Experiment"))))

    //Remove empty runIds
    val proteinGroupEntry=parseProteinGroupTable(file_prot, runIdsList1)
    val runIdsList=runIdsList1.filter(_.value.nonEmpty)

    val msmsHash= parseMaxquantMsMs(file_msms)
        //Create ProteinIdent for each entry in proteinGroupEntry
    val runIdToProtIdentList=proteinGroupEntry.flatMap({
          entry=>
            runIdsList.map({
              runId =>
              val ac:String= entry.majorityProtein(0)
              val scoreHash=obtainMsMsScoreById(entry.bestMsMs, msmsHash)
              val subset=entry.majorityProtein.drop(1).map({
                protein:String =>
                ProteinIdentInfo(AccessionCode(protein),SequenceSource(source),IdentScore(scoreHash(runId),Map()), entry.uniquePeptides(runId),entry.msCount(runId),true)
            })
            val protInfo=ProteinIdentInfo(AccessionCode(ac),SequenceSource(source),IdentScore(scoreHash(runId),Map()), entry.uniquePeptides(runId),entry.msCount(runId),true)
                Tuple2(runId,ProteinIdent(searchId,protInfo,subset))
        })
    })

    // Group the list by RunId to create a Map[RunId, List[RunId,ProteinIdent]] and then mapValues to create Map[RunId,List[ProteinIdent]]
    runIdToProtIdentList.groupBy(_._1).mapValues(list=>list.map(_._2))

  }
}
/*
    //println(mProteinsList)


    runIdsList.map ({
      runId=>
        mProteinsList.map {
          m => {
            //Obtain ACs
            val acsList: List[String]= m(mainProts).split(";").toList

            //Unique peptides
            val nSeq: Int = m(uniquePeptidesPos).toInt


            //As score we first select the best MS/MS and then we go for the msmsScans.txt to extract the corresponding score
            val bestMsMsFiles= m(msmsPos).split(";").toList(0) //we select the first id in the list

            //Find id in msms table to obtain score
            val idLineMsMsTable: List[String]= mScanMap(bestMsMsFiles)
            val score= idLineMsMsTable(scorePos).toDouble

            //Number of PSMs using MsMsCount in ProteinGroups / runId
            val msmsCountRunIdPos:Int= headerScansMap("MS/MS Count " ++ runId)
            val nPSMs= m(msmsCountRunIdPos).toInt

            //Create MaxQuant entry
            //We take the fisrt protein as main and the others as subset of proteins
            val subsetProts=acsList.drop(0).map(
              acs=>ProteinIdentInfo(AccessionCode(acs),SequenceSource(source),IdentScore(score,Map()), nSeq, nPSMs, true)
            ).toSeq
            val protInfo=ProteinIdentInfo(AccessionCode(acsList(0)),SequenceSource(source),IdentScore(score,Map()), nSeq, nPSMs, true)
            ProteinIdent(searchId,protInfo,subsetProts)

          }
        }

    })
  }

}
*/
