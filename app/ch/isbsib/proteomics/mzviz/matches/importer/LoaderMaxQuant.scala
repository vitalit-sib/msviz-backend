package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.{PeptidesTableEntry, EvidenceTableEntry, MsMsTableEntry, ProteinGroupsTableEntry}
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}

import scala.io.Source._

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object LoaderMaxQuant {

  val filename_prot = "proteinGroups.txt"
  val filename_params = "parameters.txt"
  val filename_msms = "msms.txt"
  val filename_summary = "summary.txt"
  val filename_evidence = "evidence.txt"
  val filename_peptides = "peptides.txt"


  def parseCommonLines(file: File): Tuple2[List[List[String]], Map[String, Int]] = {
    val lines: Iterator[String] = fromFile(file).getLines()
    val header = lines.take(1).next.split("\t").toList
    val range = 0 until header.length
    val headerMap: Map[String, Int] = (header zip range).toMap
    Tuple2(lines.toList.map(s => (s.split("\t")).toList), headerMap)
  }

  def getRunIds(file: File): Seq[RunId] = {
    val (mLines, headerMap) = parseCommonLines(file)
    mLines.map(row => RunId(row(headerMap("Experiment"))))
  }

  def parseProteinGroupTable(file: File, runIds: Seq[RunId]): List[ProteinGroupsTableEntry] = {

    //From proteinGroups.txt
    //Obtain position for unique peptides, MS/MS IDs, Majority proteinIDs and MS Count for runId
    val (mProteinsList, headerProtMap) = parseCommonLines(file)
    val msmsPos: Int = headerProtMap("MS/MS IDs")
    val mainProts: Int = headerProtMap("Majority protein IDs")

    val countPosHash: Map[RunId, Int] = runIds.map(runId => Tuple2(runId, headerProtMap("MS/MS Count " + runId.value))).toMap
    val uniquePepPosHash = runIds.map(runId => Tuple2(runId, headerProtMap("Unique peptides " + runId.value))).toMap

    mProteinsList.map({
      m => {
        //Obtain ACs
        val acsList: List[String] = m(mainProts).split(";").toList
        //As score we first select the best MS/MS and then we go for the msmsScans.txt to extract the corresponding score
        val bestMsMsFiles = m(msmsPos).split(";").map(_.toInt).toList

        //msCount for runId
        val msCountFiles: Map[RunId, Int] = countPosHash.mapValues(pos => (m(pos)).toInt)
        //Unique peptides
        val nSeq: Map[RunId, Int] = uniquePepPosHash.mapValues(pos => (m(pos)).toInt)

        ProteinGroupsTableEntry(bestMsMsFiles, nSeq, acsList, msCountFiles)

      }
    })
  }

  def parseMaxquantParametersTable(file: File): Map[String, String] = {

    val linesParam: Iterator[String] = fromFile(file).getLines()
    val headerParam = linesParam.take(1).next.split("\t").toList
    val paramMap: Map[String, String] = linesParam.toList.map(s => Tuple2(s.split("\t")(0), s.split("\t")(1))).toMap
    //val source= ParamMap("Fasta file")
    paramMap
  }

  /**
   *
   * @param file
   * @return Map[experiment,enzyme]
   */
  def parseMaxquantSummaryTable(file: File): Map[String, String] = {

    val linesParam: Iterator[String] = fromFile(file).getLines()
    val headerParam = linesParam.take(1).next.split("\t").toList

    val (mSummaryList, headerSummaryMap) = parseCommonLines(file)
    val experimentPos: Int = headerSummaryMap("Experiment")
    val enzymePos: Int = headerSummaryMap("Enzyme")
    val summaryMap = mSummaryList.map {
      (
        entry => Tuple2(entry(experimentPos), entry(enzymePos))
        )
    }.toMap
    summaryMap
  }

  def parseMaxquantMsMs(file: File): Map[Int, MsMsTableEntry] = {
    //List[MsMsTableEntry]

    //From msms.txt

    val headerScansMap = parseCommonLines(file)._2
    val mScansList: List[List[String]] = parseCommonLines(file)._1
    val msmsIdPos: Int = headerScansMap("id")
    val scorePos: Int = headerScansMap("Score")
    val rawFilePos: Int = headerScansMap("Raw file")

    //map from id -> info
    val msmsMap = mScansList.map({
      row =>
        val msmsEntry = MsMsTableEntry(RunId(row(rawFilePos)), row(msmsIdPos).toInt, row(scorePos).toDouble)
        Tuple2(row(msmsIdPos).toInt, msmsEntry)
    }).toMap

    msmsMap
  }

  def obtainMsMsScoreById(listIds: List[Int], msmsHash: Map[Int, MsMsTableEntry]): Map[RunId, Double] = {

    val filterByIdMsMsEntryList = msmsHash.filterKeys(listIds.toSet).values.toList
    val filterByIdMsMsEntryMapGrouped = filterByIdMsMsEntryList.groupBy(_.runId)
    val msmsEntryScores: Map[RunId, Double] = filterByIdMsMsEntryMapGrouped.mapValues(list => list.map(_.score).sum)

    //return map of RunId and sum of Scores
    msmsEntryScores
  }

  def loadProtIdent(maxQuantDir: String, runIdsList: Seq[RunId]): Map[RunId, Seq[ProteinIdent]] = {

    // load files
    val file_params = new File(maxQuantDir + filename_params)
    val file_prot = new File(maxQuantDir + filename_prot)
    val file_msms = new File(maxQuantDir + filename_msms)

    //From parameters.txt
    val source = parseMaxquantParametersTable(file_params)("Fasta file")

    val proteinGroupEntry = parseProteinGroupTable(file_prot, runIdsList)

    val msmsHash = parseMaxquantMsMs(file_msms)

    //Create ProteinIdent for each entry in proteinGroupEntry
    val runIdToProtIdentList: List[(RunId, ProteinIdent)] = proteinGroupEntry.flatMap({ entry =>

      runIdsList.map({ runId =>

        val ac: String = entry.majorityProtein(0)
        val scoreHash = obtainMsMsScoreById(entry.bestMsMs, msmsHash)

        // get scores of current runId (make it -1 if there is none for this RunId)
        val identScore = IdentScore(scoreHash.getOrElse(runId, -1), Map())

        val subset = entry.majorityProtein.drop(1).map({ protein: String =>
          ProteinIdentInfo(AccessionCode(protein), SequenceSource(source), identScore, entry.uniquePeptides(runId), entry.msCount(runId), true)
        })

        val protInfo = ProteinIdentInfo(AccessionCode(ac), SequenceSource(source), identScore, entry.uniquePeptides(runId), entry.msCount(runId), true)

        Tuple2(runId, ProteinIdent(SearchId(runId.value), protInfo, subset))
      })
    })

    // keep only results which do have a MSMS score
    val filteredRunIdToProtIdentList = runIdToProtIdentList.filter(e => e._2.mainProt.score.mainScore > 0)

    // runIdToProtIdentList.filter(e => e._2.mainProt.score.mainScore < 0).foreach(a => println(s"${a._1.value} - ${a._2.mainProt.proteinAC}"))

    // Group the list by RunId to create a Map[RunId, List[RunId,ProteinIdent]] and then mapValues to create Map[RunId,List[ProteinIdent]]
    filteredRunIdToProtIdentList.groupBy(_._1).mapValues(list => list.map(_._2))

  }

  //PepSpectraMatch object
  def parseEvidenceTable(file: File): List[EvidenceTableEntry] = {
    val (mEvidenceList, headerEvidenceMap) = parseCommonLines(file)
    val idPos: Int = headerEvidenceMap("id")
    val sequencePos: Int = headerEvidenceMap("Sequence")
    val experimentPos: Int = headerEvidenceMap("Experiment")
    val molMassPos: Int = headerEvidenceMap("Mass")
    val scorePos: Int = headerEvidenceMap("Score")
    val missedCleavagesPos: Int = headerEvidenceMap("Missed cleavages")
    val massDiffPos: Int = headerEvidenceMap("Mass Error [ppm]")
    val chargePos: Int = headerEvidenceMap("Charge")
    val acPos: Int = headerEvidenceMap("Leading Razor Protein")
    val pepIdPos: Int = headerEvidenceMap("Peptide ID")

    //Filter table, remove rows with no score, taking care about "." in the score which are not digits
    val mEvidenceListFiltered = mEvidenceList.filter({ l => l(scorePos).filter(_.isDigit).length > 0})

    mEvidenceListFiltered.map({
      m => {
        val id: Int = m(idPos).toInt
        val sequence: String = m(sequencePos)
        val experiment: String = m(experimentPos)
        val molMass: Option[Double] = Option(m(molMassPos).toDouble)
        val score: Double = m(scorePos).toDouble
        val missCleavages: Option[Int] = Option(m(missedCleavagesPos).toInt)
        val massDiff: Option[Double] = if (m(massDiffPos).filter(elem => (elem.isDigit)).length > 0) Option(m(massDiffPos).toDouble) else None
        val charge: Option[Int] = Option(m(chargePos).toInt)
        val ac: String = m(acPos)
        val pepId: Int = m(pepIdPos).toInt
        EvidenceTableEntry(id, sequence, experiment, molMass, score, missCleavages, massDiff, charge, ac, pepId)
      }
    })
  }

  def parsePeptidesTable(file: File, runIds: Seq[RunId]): Map[Int, PeptidesTableEntry] = {
    val (mPeptidesList, headerPeptidesMap) = parseCommonLines(file)
    //val evidenceIdPos: Int = headerPeptidesMap("Evidence IDs")
    val peptideIdPos: Int = headerPeptidesMap("id")
    val previousAAPos: Int = headerPeptidesMap("Amino acid before")
    val nextAAPos: Int = headerPeptidesMap("Amino acid after")
    val startPos: Int = headerPeptidesMap("Start position")
    val endPos: Int = headerPeptidesMap("End position")
    val isDecoyPos: Int = headerPeptidesMap("Reverse")

    //Filter mPeptidesList, remove entries with no start or end position, coming from REV_
    val mPeptidesListFiltered = mPeptidesList.filter({ l => !l(startPos).isEmpty() && !l(endPos).isEmpty()})

    //map from pepId -> info
    val peptidesMap = mPeptidesListFiltered.map({
      row =>
        val pepId = row(peptideIdPos).toInt
        val isDecoy = if (row(isDecoyPos).isEmpty()) Option(false) else Option(true)
        val peptidesEntry = PeptidesTableEntry(pepId, Option(row(previousAAPos)), Option(row(nextAAPos)), row(startPos).toInt,
          row(endPos).toInt, isDecoy)
        Tuple2(pepId, peptidesEntry)
    }).toMap
    peptidesMap
  }

  def loadPepSpectraMatch(maxQuantDir: String, runIds: Seq[RunId]): Map[RunId, Seq[PepSpectraMatch]] = {
    //load files
    val file_evidence = new File(maxQuantDir + filename_evidence)
    val file_peptides = new File(maxQuantDir + filename_peptides)

    val peptidesHash = parsePeptidesTable(file_peptides, runIds)
    val evidenceEntryAux: List[EvidenceTableEntry] = parseEvidenceTable(file_evidence)
    //Filter evidenceEntryAux, remove rows where id doesn't correspond to any key in PeptidesHash
    val evidenceEntry: List[EvidenceTableEntry] = evidenceEntryAux.filter({ entry => peptidesHash.keySet.exists(_ == entry.id)})



    //Create PepSpectraMatch for each entry in evidenceEntry
    val runIdToPepSpectraMatchList: List[(RunId, PepSpectraMatch)] = evidenceEntry.map({ entry =>
      val pep = Peptide(entry.sequence, entry.molMass, Vector(Seq(ModifName(""))))
      val spectrumId = SpectrumId(SpectrumUniqueId(entry.pepId.toString), RunId(entry.experiment))
      val matchInfo = PepMatchInfo(IdentScore(entry.score, Map()), entry.missedCleavages, entry.massDiff, None, None, entry.chargeState, None)

      // @TODO we will have to add the source somehow
      val leadingProteinRef = ProteinRef(AccessionCode(entry.ac), Set(), None)
      val pepEntry = peptidesHash(entry.pepId)

      val proteinList = Seq(ProteinMatch(leadingProteinRef, pepEntry.previousAA, pepEntry.nextAA, pepEntry.startPos, pepEntry.endPos, pepEntry.isDecoy))

      Tuple2(RunId(entry.experiment), PepSpectraMatch(SearchId(entry.experiment), spectrumId, pep, matchInfo, proteinList))
    })

    // Group the list by RunId to create a Map[RunId, List[RunId,PepSpectraMatch]] and then mapValues to create Map[RunId,List[PepSpectraMatch]]
    runIdToPepSpectraMatchList.groupBy(_._1).mapValues(list => list.map(_._2))
  }

  /**
   * parse a maxquant file and return search information.
   * @param maxQuantDir
   * @return
   */
  def parseSearchInfo(maxQuantDir: String): Map[RunId, SearchInfo] = {

    // load files
    val file_params = new File(maxQuantDir + filename_params)
    val file_summary = new File(maxQuantDir + filename_summary)

    val paramsHash = parseMaxquantParametersTable(file_params)
    val summaryHash: Map[String, String] = parseMaxquantSummaryTable(file_summary)

    val database = paramsHash("Fasta file")


    val username = paramsHash("User name")
    //TOCHANGE
    val parentTolerance = -1
    val fragmentTolerance = paramsHash("MS/MS tol. (FTMS)")

    val searchInfoHashAux: Map[RunId, SearchInfo] = summaryHash.map({
      keyVal => Tuple2(RunId(keyVal._1), SearchInfo(SearchId(keyVal._1), keyVal._1, Seq(SearchDatabase(database, None, None)), username, keyVal._2, parentTolerance.toString, fragmentTolerance))
    })

    val searchInfoHash= searchInfoHashAux.filter({entry=> entry._1.toString !="RunId()"})
    println(searchInfoHash.keys)
    searchInfoHash
  }


  def parse(maxQuantDir: String): Seq[Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo]] = {

    //parse summary.txt to obtain List(RunId)
    val runIdsWithEmpty: Seq[RunId] = LoaderMaxQuant.getRunIds(new File("test/resources/maxquant/summary.txt"))
    val runIds = runIdsWithEmpty.filter(_.value.nonEmpty)

    val psmList:Map[RunId,Seq[PepSpectraMatch]] = loadPepSpectraMatch(maxQuantDir, runIds)
    val proteinList = loadProtIdent(maxQuantDir, runIds)
    val searchInfo = parseSearchInfo(maxQuantDir)

    val tupleSeq= runIds.map({
      runId=>  Tuple3(psmList(runId), proteinList(runId), searchInfo(runId))
    })
    tupleSeq
  }
}

