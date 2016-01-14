package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.{IOException, File}

import ch.isbsib.proteomics.mzviz.commons.helpers.Unzip
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


  def parseCommonLines(file: File): (List[List[String]], Map[String, Int]) = {
    val lines: Iterator[String] = fromFile(file).getLines()
    val header = lines.take(1).next.split("\t").toList
    val range = 0 until header.length
    val headerMap: Map[String, Int] = (header zip range).toMap
    Tuple2(lines.toList.map(s => (s.split("\t")).toList), headerMap)
  }

  def getRunIds(file: File): Seq[(RunId, String)] = {
    val (mLines, headerMap) = parseCommonLines(file)
    val runIdsAndRawfilesWithEmpties: Seq[(RunId, String)] = mLines.map(row => Tuple2(RunId(row(headerMap("Experiment"))), row(headerMap("Raw file"))))
    // remove empty entries
    runIdsAndRawfilesWithEmpties.filter(_._1.value.nonEmpty)
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

  def parseFastaFilename(fastaFilename:String): String = {
    val fastaFileNameRegx = """.+\\(.+).fasta""".r
    val fileNameRegx = """.+\\(.+)""".r

    fastaFilename match {
      case fastaFileNameRegx(n) => n
      case fileNameRegx(n) => n
      case _ => fastaFilename
    }
  }

  def parseMaxquantParametersTable(file: File): Map[String, String] = {

    val linesParam: Iterator[String] = fromFile(file).getLines()
    //val headerParam = linesParam.take(1).next.split("\t").toList
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
    //val headerParam = linesParam.take(1).next.split("\t").toList

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

  def parseMaxquantMsMs(file: File, rawfilesRunIdMap: Map[String, RunId]): Map[Int, MsMsTableEntry] = {

    //From msms.txt

    val headerScansMap = parseCommonLines(file)._2
    val mScansList: List[List[String]] = parseCommonLines(file)._1
    val msmsIdPos: Int = headerScansMap("id")
    val scorePos: Int = headerScansMap("Score")
    val rawFilePos: Int = headerScansMap("Raw file")

    //map from id -> info
    val msmsMap = mScansList.map({
      row =>
        val msmsEntry = MsMsTableEntry(rawfilesRunIdMap(row(rawFilePos)), row(msmsIdPos).toInt, row(scorePos).toDouble)
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

  def loadProtIdent(maxQuantDir: String, runIdsAndRawfiles: Seq[(RunId, String)], sequenceSource:SequenceSource): Map[RunId, Seq[ProteinIdent]] = {

    // load files
    val file_prot = new File(maxQuantDir + filename_prot)
    val file_msms = new File(maxQuantDir + filename_msms)

    val runIdsList = runIdsAndRawfiles.map(_._1)
    val proteinGroupEntry = parseProteinGroupTable(file_prot, runIdsList)

    val rawfilesRunIdMap: Map[String, RunId] = runIdsAndRawfiles.map(t => Tuple2(t._2, t._1)).toMap
    val msmsHash = parseMaxquantMsMs(file_msms, rawfilesRunIdMap)

    //Create ProteinIdent for each entry in proteinGroupEntry
    val runIdToProtIdentList: List[(RunId, ProteinIdent)] = proteinGroupEntry.flatMap({ entry =>

      runIdsList.map({ runId =>

        val ac: String = entry.majorityProtein(0)
        val scoreHash = obtainMsMsScoreById(entry.bestMsMs, msmsHash)

        // get scores of current runId (make it -1 if there is none for this RunId)
        val identScore = IdentScore(scoreHash.getOrElse(runId, -1), Map())

        val subset = entry.majorityProtein.drop(1).map({ protein: String =>
          ProteinIdentInfo(AccessionCode(protein), sequenceSource, identScore, entry.uniquePeptides(runId), entry.msCount(runId), true)
        })

        val protInfo = ProteinIdentInfo(AccessionCode(ac), sequenceSource, identScore, entry.uniquePeptides(runId), entry.msCount(runId), true)

        Tuple2(runId, ProteinIdent(SearchId(runId.value), protInfo, subset))
      })
    })


    // keep only results which do have a MSMS score
    val filteredRunIdToProtIdentList = runIdToProtIdentList.filter(e => e._2.mainProt.score.mainScore > 0)

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
    val acPos: Int = if(headerEvidenceMap.contains("Leading Razor Protein")) headerEvidenceMap("Leading Razor Protein") else headerEvidenceMap("Leading razor protein")
    val pepIdPos: Int = headerEvidenceMap("Peptide ID")
    val modifSeqPos: Int= headerEvidenceMap("Modified sequence")
    val modifNamePos: Int= headerEvidenceMap("Modifications")
    val lengthPos: Int = headerEvidenceMap("Length")

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
        val modificationSeq: String = m(modifSeqPos)
        val modificationsList: Seq[String] = m(modifNamePos).split(",")
        val hashModMaxQuantUnimod: Map[String,String]= createHashModMaxQuantUnimod(modificationsList)
        val modifNamesMaxQuant: List[String] = """\([a-z]*\)""".r.findAllIn(modificationSeq).toList
        val modifPosMaxQuant: List[Int] = findModificationPosRecursive(modifNamesMaxQuant,modificationSeq)
        val hashPosModificationMaxQ= createHashPosModificationMaxQ(modifPosMaxQuant,modifNamesMaxQuant)
        val hashPosModification= createHashPosModification(modifPosMaxQuant,hashPosModificationMaxQ,hashModMaxQuantUnimod)
        val lenghtVector=(m(lengthPos).toInt)+2
        val vectorNames= Vector.fill(lenghtVector)(Seq())

        //Check if there is any modification
        if(hashPosModification.keys != Set()) {
          val modifNamesVector: Vector[Seq[ModifName]] = updateVector(hashPosModification,lenghtVector)
          EvidenceTableEntry(id, sequence, experiment, molMass, score, missCleavages, massDiff, charge, ac, pepId,modifNamesVector)
        }
        else EvidenceTableEntry(id, sequence, experiment, molMass, score, missCleavages, massDiff, charge, ac, pepId,vectorNames)

      }
    })
  }

  def updateVector(modifPosHash: Map[Int, String], vectorLength: Int): Vector[Seq[ModifName]] = {
    (1 to vectorLength).toVector.map({pos =>
      if(modifPosHash.contains(pos)){
        Seq(ModifName(modifPosHash(pos)))
      }else{
        Seq()
      }
    })
  }

  //Create a hash with the position and the modification with Unimod name
  def createHashPosModification (modificationPosList: List[Int], hashPosModificationMaxQ:Map[Int, String] , hashModMaxQuantUnimod:Map[String,String]): Map[Int, String] = {
    val hashPosModif= modificationPosList.map({
      pos=>
        Tuple2(pos, hashModMaxQuantUnimod(hashPosModificationMaxQ(pos)))
    }).toMap
    hashPosModif
  }

  //Create a hash with the position and the modification with MaxQuant name
  def createHashPosModificationMaxQ (modificationPosList: List[Int],modifNamesMaxQuant: List[String]): Map[Int, String] = {

    //First remove () for each modification
    val modifNamesMaxQuantFinal: List[String] = modifNamesMaxQuant.map({
      mod =>
        val toRemove= "()".toSet
        mod.filterNot(toRemove)
    })
    (modificationPosList zip modifNamesMaxQuantFinal).toMap
  }

  //Create a hash with MaxQuant modification names and their correspondant name for Unimod, i.e ox-> Oxidation
  def createHashModMaxQuantUnimod (modificationList: Seq[String]): Map[String,String] ={
    val modifFiltered: Map[String,String]=modificationList.map({
      name=>
        val filteredName= """[A-Z][a-z]*""".r.findFirstIn(name)
        Tuple2(filteredName.get.toLowerCase().substring(0,2),filteredName.get)
    }).toMap
    modifFiltered
  }

  //Obtain position for every modification inside the sequence
  def findModificationPosRecursive(modifList:List[String],sequence:String): List[Int] ={
    def recur(modifList:List[String],sequence:String, indexList:List[Int]) :List[Int] = {
      modifList.length match{
        case 0 => indexList
        case length => {
          recur(modifList.tail,sequence.replaceFirst("""\(""" + modifList(0) + """\)""",""), indexList :+ sequence.indexOf(modifList(0)))
        }
      }
    }
    recur(modifList,sequence,List())
  }
  def parsePeptidesTable(file: File): Map[Int, PeptidesTableEntry] = {
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

  def loadPepSpectraMatch(maxQuantDir: String, runIds: Seq[RunId], sequenceSource:SequenceSource): Map[RunId, Seq[PepSpectraMatch]] = {
    //load files
    val file_evidence = new File(maxQuantDir + filename_evidence)
    val file_peptides = new File(maxQuantDir + filename_peptides)

    val peptidesHash = parsePeptidesTable(file_peptides)
    val evidenceEntryAux: List[EvidenceTableEntry] = parseEvidenceTable(file_evidence)
    //Filter evidenceEntryAux, remove rows where id doesn't correspond to any key in PeptidesHash
    val evidenceEntry: List[EvidenceTableEntry] = evidenceEntryAux.filter({ entry => peptidesHash.keySet.exists(_ == entry.id)})



    //Create PepSpectraMatch for each entry in evidenceEntry
    val runIdToPepSpectraMatchList: List[(RunId, PepSpectraMatch)] = evidenceEntry.map({ entry =>
      val pep = Peptide(entry.sequence, entry.molMass, entry.modificationVector)
      val spectrumId = SpectrumId(SpectrumUniqueId(entry.pepId.toString), RunId(entry.experiment))
      val matchInfo = PepMatchInfo(IdentScore(entry.score, Map()), entry.missedCleavages, entry.massDiff, None, None, entry.chargeState, None)

      val leadingProteinRef = ProteinRef(AccessionCode(entry.ac), Set(), Some(sequenceSource))
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
  def parseSearchInfo(maxQuantDir: String, sequenceSource:SequenceSource): Map[RunId, SearchInfo] = {

    // load files
    val file_params = new File(maxQuantDir + filename_params)
    val file_summary = new File(maxQuantDir + filename_summary)

    val paramsHash = parseMaxquantParametersTable(file_params)
    val summaryHash: Map[String, String] = parseMaxquantSummaryTable(file_summary)

    val username = paramsHash("User name")
    val parentTolerance = None
    val fragmentTolerance = paramsHash("MS/MS tol. (FTMS)")

    val searchInfoHashAux: Map[RunId, SearchInfo] = summaryHash.map({
      keyVal => Tuple2(RunId(keyVal._1), SearchInfo(SearchId(keyVal._1), keyVal._1, Seq(SearchDatabase(sequenceSource.value, None, None)), username, keyVal._2, parentTolerance, fragmentTolerance))
    })

    val searchInfoHash= searchInfoHashAux.filter({entry=> entry._1.toString !="RunId()"})
    searchInfoHash
  }


  def parse(maxQuantDir: String): Seq[(Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo)] = {

    val file_params = new File(maxQuantDir + filename_params)

    //From parameters.txt
    val source = parseMaxquantParametersTable(file_params)("Fasta file")
    val sequenceSource = SequenceSource(parseFastaFilename(source))

    //parse summary.txt to obtain List(RunId)
    val runIdsAndRawfiles: Seq[(RunId, String)] = LoaderMaxQuant.getRunIds(new File(maxQuantDir + filename_summary))
    val runIds = runIdsAndRawfiles.map(_._1)

    val psmList: Map[RunId,Seq[PepSpectraMatch]] = loadPepSpectraMatch(maxQuantDir, runIds, sequenceSource)
    val proteinList: Map[RunId, Seq[ProteinIdent]] = loadProtIdent(maxQuantDir, runIdsAndRawfiles, sequenceSource)
    val searchInfo: Map[RunId, SearchInfo] = parseSearchInfo(maxQuantDir, sequenceSource)

    val tupleSeq= runIds.map({
      runId =>  Tuple3(psmList(runId), proteinList(runId), searchInfo(runId))
    })
    tupleSeq
  }


  def parseZip(maxQuantZip: File): Seq[(Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo)] = {
    val tmpDir = Unzip.unzip(maxQuantZip)

    val dirsInTmpDir = Unzip.getListOfDirs(tmpDir)
    val txtDir = if(dirsInTmpDir.length == 1 && dirsInTmpDir(0).getName == "txt") tmpDir + "/txt/" else tmpDir + "/"

    parse(txtDir)

    // remove tmpDir once we finished parsing (we will have to add scala-io to do so)
    //val tmpPath:Path = Path(tmpDir)
    //Try(tmpPath.deleteRecursively(continueOnFailure = true))

  }


}

