package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.ProteinGroupsTable
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


  def parseProteinGroupTable(file: File, runId: RunId): List[ProteinGroupsTable] = {

    //From proteinGroups.txt
    val linesProt: Iterator[String] = fromFile(file).getLines()
    val headerProt = linesProt.take(1).next.split("\t").toList
    val rangeProt = 0 until headerProt.length
    val headerProtMap: Map[String, Int] = (headerProt zip rangeProt).toMap
    val mProteinsList: List[List[String]] = linesProt.toList.map(s => (s.split("\t")).toList)
    //Obtein position for unique peptides, best MS/MS,Majority proteinIDs and MS Count for runiD
    val uniquePeptidesPos: Int = headerProtMap("Unique peptides")
    val msmsPos: Int = headerProtMap("Best MS/MS")
    val mainProts: Int = headerProtMap("Majority protein IDs")
    val msCountPos: Int = headerProtMap("MS/MS Count " + runId.value)

    mProteinsList.map({
      m => {
        //Obtain ACs
        val acsList: List[String] = m(mainProts).split(";").toList
        //Unique peptides
        val nSeq: Int = m(uniquePeptidesPos).toInt
        //As score we first select the best MS/MS and then we go for the msmsScans.txt to extract the corresponding score
        val bestMsMsFiles = m(msmsPos).split(";").toList
        //msCount for runId
        val msCountFiles = m(msCountPos).toInt

        ProteinGroupsTable(bestMsMsFiles, nSeq, acsList, msCountFiles, runId)

      }
    })
  }

  def loadProtMaxQuant(file: File, searchId: SearchId): List[List[ProteinGroupsTable]] = {

    //TOCHANGE
    //Seq[Seq[ProteinIdent]]

    //Obtain list of runIDs

    //From proteinGroups.txt
    val linesSummary: Iterator[String] = fromFile(file_summary).getLines()
    val headerSummary = linesSummary.take(1).next.split("\t").toList
    val rangeSummary= 0 until headerSummary.length
    val headerSummaryMap: Map[String, Int]= (headerSummary zip rangeSummary).toMap
    val mSummaryList:List[List[String]]  = linesSummary.toList.map(s => (s.split("\t")).toList)
    val runIdsList1: List[String] = mSummaryList.map(row=>row(headerSummaryMap("Experiment")))

    //Remove empty runIds
    val runIdsList=runIdsList1.filter(_.nonEmpty)
    runIdsList.map({
      runId =>
        parseProteinGroupTable(file_prot, RunId(runId))
    })
  }
}
/*
    //println(mProteinsList)

    //From parameters.txt
    val linesParam: Iterator[String] = fromFile(file_params).getLines()
    val headerParam = linesParam.take(1).next.split("\t").toList
    val ParamMap: Map[String,String]= linesParam.toList.map(s => Tuple2(s.split("\t")(0),s.split("\t")(1))).toMap
    val source= ParamMap("Fasta file")

    //From msms.txt
    val linesScans: Iterator[String] = fromFile(file_msms).getLines()
    val headerScans = linesScans.take(1).next.split("\t").toList
    val rangeScans= 0 until headerScans.length
    val headerScansMap= (headerScans zip rangeScans).toMap
    val mScansList:List[List[String]]  = linesScans.toList.map(s => (s.split("\t")).toList)
    //map from id -> info
    val listIdInfo= mScansList(headerScansMap("id"))
    val mScanMap:Map[String,List[String]] = (listIdInfo zip mScansList).toMap



    val msmsId: Int= headerScansMap("id")
    val scorePos: Int= headerScansMap("score")


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
