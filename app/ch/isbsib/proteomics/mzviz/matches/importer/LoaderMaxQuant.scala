package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}

import scala.io.Source._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object LoaderMaxQuant {

  val file_prot = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/proteinGroups.txt")
  val file_evidence = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/evidence.txt")
  val file_params = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/parameters.txt")
  val file_msms = new File("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/maxquant/msms.txt")

  def LoadProt (file:File,searchId: SearchId) {


    //To return seq[ProteinIndentMaxQuant]

    //Obtain list of runIDs  TOCHANGE
    val runIdsList: List[String]= List("F001644")

    //From proteinGroups.txt
    val linesProt: Iterator[String] = fromFile(file_prot).getLines()
    val headerProt = linesProt.take(1).next.split("\t").toList
    val rangeProt= 0 until headerProt.length
    val headerProtMap: Map[String, Int]= (headerProt zip rangeProt).toMap
    val mProteinsList:List[List[String]] = linesProt.toList.map(s => (s.split("\t")).toList)
    //println(mProteinsList)

    //From evidence.txt
    val linesEvidence: Iterator[String] = fromFile(file_evidence).getLines()
    val headerEvidence = linesEvidence.take(1).next.split("\t").toList
    val rangeEvidence= 0 until headerEvidence.length
    val headerEvidenceMap= (headerEvidence zip rangeEvidence).toMap
    val mEvidenceList:List[List[String]]  = linesEvidence.toList.map(s => (s.split("\t")).toList)

    //Map of id (Evidence it to link with the proteinGroups.txt) and the whole list of evidence.txt info
    val listEvidenceIdInfo= mEvidenceList(headerEvidenceMap("id"))
    val mEvidenceMap:Map[String,List[String]] = (listEvidenceIdInfo zip mEvidenceList).toMap

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


    //Obtein position for unique peptides, best MS/MS, MS scan number, Majority proteinIDs
    val uniquePeptidesPos:Int = headerProtMap("Unique peptides")
    val msmsPos: Int= headerProtMap("Best MS/MS")
    val mainProts: Int= headerProtMap("Majority protein IDs")
    val msmsId: Int= headerScansMap("id")
    val scorePos: Int= headerScansMap("score")


    runIdsList.map {
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
            val protein= ProteinIdent(searchId,protInfo,subsetProts)

          }
        }
    }

  }

}
