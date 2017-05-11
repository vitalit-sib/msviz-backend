package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.{File, IOException}
import java.util.Calendar

import ch.isbsib.proteomics.mzviz.commons.{MolecularMass, Moz, PPM}
import ch.isbsib.proteomics.mzviz.commons.helpers.{CommonFunctions, FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.experimental.{RunId, ScanNumber, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.{EvidenceTableEntry, MsMsTableEntry, PeptidesTableEntry, ProteinGroupsTableEntry}
import ch.isbsib.proteomics.mzviz.matches.services.SearchInfoDBService
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}

import scala.io.Source._
import scala.util.{Failure, Try}


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2017, SIB Swiss Institute of Bioinformatics
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

    val countPosHash: Map[RunId, Int] = runIds.map({ runId =>
      val msmsCount = if(headerProtMap.contains("MS/MS Count " + runId.value)) {
        headerProtMap("MS/MS Count " + runId.value)
      } else {
        headerProtMap("MS/MS Count")
      }
      Tuple2(runId, msmsCount)
    }).toMap
    val uniquePepPosHash = runIds.map(runId => Tuple2(runId, headerProtMap("Unique peptides " + runId.value))).toMap

    // filter out entries without a corresponding msmsId
    val filteredProteinsList = mProteinsList.filter(_.length > msmsPos)

    filteredProteinsList.map({
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
    //Prepared for several sources
    val fastaFilesArray= fastaFilename.split(";")

    // Create a new String separated by ";" with the source names
    fastaFilesArray.map{(
      file => {
        val a=file.split("\\\\") //(file.length - 1)
        a(a.length - 1)}
      )}.mkString(";")
  }

  def parseMaxquantParametersTable(file: File): Map[String, String] = {

    val linesParam: Iterator[String] = fromFile(file).getLines()
    //val headerParam = linesParam.take(1).next.split("\t").toList
    val splittedLines: List[Array[String]] = linesParam.toList.map(_.split("\t"))
    val paramMap: Map[String, String] =  splittedLines.filter(_.length == 2).map( s => (s(0), s(1))).toMap
    paramMap
  }

  /**
   *
   * @param file
   * @return Map[experiment,(enzyme, raw file)]
   */
  def parseMaxquantSummaryTable(file: File): Map[String, (String, String)] = {

    val (mSummaryList, headerSummaryMap) = parseCommonLines(file)

    val experimentPos: Int = headerSummaryMap("Experiment")
    val enzymePos: Int = headerSummaryMap("Enzyme")
    val rawFilePos: Int = headerSummaryMap("Raw file")

    val summaryMap = mSummaryList.map {(
        entry => Tuple2(entry(experimentPos), (entry(enzymePos), entry(rawFilePos)))
      )}.toMap
    summaryMap
  }
  /**
   *
   * @param file
   * @return Map[raw,experiment]
   */
  def parseMaxquantSummaryTableRawSearchId(file: File): Map[String, String] = {

    val (mSummaryList, headerSummaryMap) = parseCommonLines(file)
    val experimentPos: Int = headerSummaryMap("Experiment")
    val rawPos: Int = headerSummaryMap("Raw file")
    val summaryMap = mSummaryList.map {
      (
        entry => Tuple2(entry(rawPos),entry(experimentPos))

        )
    }.filterNot(_._2.isEmpty()).toMap
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

  def loadProtIdent(maxQuantDir: String, runIdsAndRawfiles: Seq[(RunId, String)], sequenceSource:SequenceSource, idTitle:Option[String]): Map[RunId, Seq[ProteinIdent]] = {

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

        // check if we have a contaminant
        val (corrAc, isContaminant) = if(ac.contains("CON__")){
          (ac.replaceFirst("CON__", ""), Some(true))
        }else{
          (ac, Some(false))
        }

        val scoreHash = obtainMsMsScoreById(entry.bestMsMs, msmsHash)

        // get scores of current runId (make it -1 if there is none for this RunId)
        val identScore = IdentScore(scoreHash.getOrElse(runId, -1), Map())

        val subset = entry.majorityProtein.drop(1).map({ protein: String =>
          ProteinIdentInfo(AccessionCode(protein), sequenceSource, identScore, entry.uniquePeptides(runId), entry.msCount(runId), true, isContaminant)
        })

        val protInfo = ProteinIdentInfo(AccessionCode(corrAc), sequenceSource, identScore, entry.uniquePeptides(runId), entry.msCount(runId), true, isContaminant)

        val searchId = if(idTitle.isDefined) idTitle.get + runId.value else runId.value
        Tuple2(runId, ProteinIdent(SearchId(searchId), protInfo, subset))
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
    val sequencePos: Int = if(headerEvidenceMap.contains("Sequence")) headerEvidenceMap("Sequence") else headerEvidenceMap("Diff")
    val experimentPos: Int = headerEvidenceMap("Experiment")
    val molMassPos: Int = headerEvidenceMap("Mass")
    val mozPos: Int = headerEvidenceMap("m/z")
    val scorePos: Int = headerEvidenceMap("Score")
    val missedCleavagesPos: Int = headerEvidenceMap("Missed cleavages")
    val typePos: Int = headerEvidenceMap("Type")
    val massDiffPos: Int = headerEvidenceMap("Mass Error [ppm]")
    val chargePos: Int = headerEvidenceMap("Charge")
    val acPos: Int = if(headerEvidenceMap.contains("Leading Razor Protein")) headerEvidenceMap("Leading Razor Protein") else headerEvidenceMap("Leading razor protein")
    val pepIdPos: Int = headerEvidenceMap("Peptide ID")
    val modifSeqPos: Int= headerEvidenceMap("Modified sequence")
    val modifNamePos: Int= headerEvidenceMap("Modifications")
    val lengthPos: Int = headerEvidenceMap("Length")
    val scanNumberPos: Int= headerEvidenceMap("MS/MS Scan Number")

    // get the fields concerning position probabilites
    val positionProbKeys = headerEvidenceMap.keySet.filter(_ matches ".+\\s+Probabilities")
    val modifProbSet: Set[(String, Int)] = positionProbKeys.map(_ replace("""(\w+)\(\w+\)\s+Probabilities""", "'$1")).zip(positionProbKeys.map(k => headerEvidenceMap(k)))

    //Filter table, remove rows with no score, taking care about "." in the score which are not digits
    val mEvidenceListFiltered = mEvidenceList.filter({ l => l(scorePos).filter(_.isDigit).length > 0})

    // keep only entries with "MULTI-MSMS"
    val mEvidenceListFiltered2 = mEvidenceListFiltered.filter({ l => l(typePos) == "MULTI-MSMS"})

    mEvidenceListFiltered2.map({
      m => {
        val id: Int = m(idPos).toInt
        val sequence: String = m(sequencePos)
        val experiment: String = m(experimentPos)
        val molMass: Option[Double] = Option(m(molMassPos).toDouble)
        val score: Double = m(scorePos).toDouble
        val missCleavages: Option[Int] = Try(m(missedCleavagesPos).toInt).toOption
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
        val scanNumber:Int= m(scanNumberPos).toInt
        val correctedMoz: Option[Double] = Try(m(mozPos).toDouble).toOption
        val correctedMolMass: Option[Double] = if(correctedMoz.isDefined && charge.isDefined) Some((correctedMoz.get * charge.get) - (CommonFunctions.PROTON_MASS * charge.get)) else None

        // check if we have a contaminant
        val (corrAc, isContaminant) = if(ac.contains("CON__")){
          (ac.replaceFirst("CON__", ""), Some(true))
        }else{
          (ac, Some(false))
        }

        //Check if there is any modification
        if(hashPosModification.keys != Set()) {
          val modifNamesVector: Vector[Seq[ModifName]] = updateVector(hashPosModification,lenghtVector)

          // parse the position probabilities
          val modifProbs: Map[ModifName, String] = parseModifProbs(m, modifProbSet)
          val highestModifProb: Map[ModifName, Double] = parseHighestModifProb(modifProbs)

          val modifInfos: Map[ModifName, Seq[ModifInfo]] = parseModificationProbabilityInfo(modifProbs, modifNamesVector)

          // check if we have an N-term modif like Acetyl (in this case there is no probability string)
          val nTermModifName:Map[ModifName, Seq[ModifInfo]]  = hashPosModification.find(mod => mod._1 == 1).map({mod =>
            val perfectModifInfo = Seq(ModifInfo(
              position = 1,
              // the probability is 1.0, since there is only 1 possible position
              modifProb = 1.0,
              status = MAIN
            ))
            (ModifName(mod._2) -> perfectModifInfo)
          }).toMap

          val allModifInfos: Map[ModifName, Seq[ModifInfo]] = modifInfos ++ nTermModifName

          EvidenceTableEntry(id, sequence, experiment, molMass, correctedMoz, correctedMolMass, score, missCleavages, massDiff, charge, corrAc, pepId,modifNamesVector, modifProbs, highestModifProb, allModifInfos, scanNumber, isContaminant)
        }
        else EvidenceTableEntry(id, sequence, experiment, molMass, correctedMoz, correctedMolMass, score, missCleavages, massDiff, charge, corrAc, pepId,vectorNames, Map(), Map(), Map(), scanNumber, isContaminant)

      }
    })
  }

  /**
    * parse the whole string containing the modification probabilites
    *
    * @param l
    * @param modifProbSet
    */
  def parseModifProbs(l: List[String], modifProbSet: Set[(String, Int)]): Map[ModifName, String] = {

    modifProbSet.foldLeft(Map.empty[ModifName, String]){
      case (a, b) =>
        val seq = l(b._2)
        val modifName = """^(\w+)""".r.findFirstIn(b._1).get
        if(! seq.isEmpty) a ++ Map(ModifName(modifName) -> seq) else a
    }

  }


  /**
    * parse the highest probability from the string
    *
    * @param modifProbs
    * @return
    */
  def parseHighestModifProb(modifProbs:  Map[ModifName, String]):  Map[ModifName, Double] = {

      modifProbs.map({ case (modif: ModifName, seq: String) =>
        val maxProb = """([\d|\.]+)""".r.findAllMatchIn(seq).map(_.group(1).toDouble).max
        (modif, maxProb)
      })
  }


  /**
    * recursive function to parse the position and probabilities from a string like: VVES(0.977)PDFS(0.023)KDEDYLGK
    * @param s
    * @param offset
    * @return
    */
  def parseModifString(s:String, offset:Int):Seq[(Int, Double)] = {
    val i = s.indexOf("(")
    if(i>=0){
      val p = "[\\d|\\.]+".r.findFirstIn(s).get.toDouble
      val newString = s.substring(s.indexOf(")") + 1)
      (i + offset, p) +: parseModifString(newString, i + offset)
    }else{
      Nil
    }
  }

  /**
    * parse modification infos
    * @param modifProbs
    * @param modifNamesVector
    * @return
    */
  def parseModificationProbabilityInfo(modifProbs:  Map[ModifName, String], modifNamesVector: Vector[Seq[ModifName]]): Map[ModifName, Seq[ModifInfo]] = {
      val splittedModifs = modifProbs.map({ case (modif: ModifName, seq: String) => (modif, parseModifString(seq, 0)) })

      splittedModifs.map({ case(modif:ModifName, info:Seq[(Int, Double)]) =>
        val infoSeq = info.map({ i =>
          val status = if(modifNamesVector(i._1).contains(modif)) MAIN else CONFLICT
          ModifInfo(position = i._1, modifProb = i._2, status = status)
        })
        (modif -> infoSeq)
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
  def parsePeptidesTable(file: File): (Map [Int, Int] ,Map[Int, PeptidesTableEntry]) = {
    val (mPeptidesList, headerPeptidesMap) = parseCommonLines(file)
    val evidenceIdPos: Int = headerPeptidesMap("Evidence IDs")
    val peptideIdPos: Int = headerPeptidesMap("id")
    val previousAAPos: Int = headerPeptidesMap("Amino acid before")
    val nextAAPos: Int = headerPeptidesMap("Amino acid after")
    val startPos: Int = headerPeptidesMap("Start position")
    val endPos: Int = headerPeptidesMap("End position")
    val isDecoyPos: Int = headerPeptidesMap("Reverse")

    //Filter mPeptidesList, remove entries with no start or end position, coming from REV_
    val mPeptidesListFiltered = mPeptidesList.filter({ l => !l(startPos).isEmpty() && !l(endPos).isEmpty()})

    val emptyEvidencePeptideMap:Map[Int, Int] = Map()
    val emptyPeptideMap:Map[Int, PeptidesTableEntry] = Map()


    val evidencePeptidesResult=mPeptidesListFiltered.foldLeft(Tuple2(emptyEvidencePeptideMap, emptyPeptideMap))({ (a, row) =>
      val pepId = row(peptideIdPos).toInt
      val evidenceId = row(evidenceIdPos)
      val isDecoy = if (row(isDecoyPos).isEmpty()) Option(false) else Option(true)

      //We create a new hashmap to link evidence Id with pep Id
      val evidencePeptideMap = if(evidenceId != "") evidenceId.split(";").map ({
        evidId => Tuple2(evidId.toInt,pepId)
      }).toMap else Map()

      val peptidesEntry = PeptidesTableEntry(pepId, Option(row(previousAAPos)), Option(row(nextAAPos)), row(startPos).toInt,
        row(endPos).toInt, isDecoy)

      val peptideMap:Map[Int, PeptidesTableEntry] = Map(pepId -> peptidesEntry)

      Tuple2(a._1 ++ evidencePeptideMap, a._2 ++ peptideMap)
    })
    evidencePeptidesResult

  }

  def loadPepSpectraMatch(maxQuantDir: String, runIds: Seq[RunId], sequenceSource:SequenceSource, idTitle:Option[String]): Map[RunId, Seq[PepSpectraMatch]] = {
    //load files
    val file_evidence = new File(maxQuantDir + filename_evidence)
    val file_peptides = new File(maxQuantDir + filename_peptides)

    val (evidencePepHash,peptidesHash) = parsePeptidesTable(file_peptides)
    val evidenceEntryAux: List[EvidenceTableEntry] = parseEvidenceTable(file_evidence)
    //Filter evidenceEntryAux, remove rows where id doesn't correspond to any key in evidencePepHash
    val evidenceEntry: List[EvidenceTableEntry] = evidenceEntryAux.filter({ entry => evidencePepHash.keySet.exists(_ == entry.id)})

    //Create PepSpectraMatch for each entry in evidenceEntry
    val runIdToPepSpectraMatchList: List[(RunId, PepSpectraMatch)] = evidenceEntry.map({ entry =>
      val pep = Peptide(entry.sequence, entry.molMass, entry.modificationVector)
      val spectrumId = SpectrumId(SpectrumUniqueId(entry.scanNumber.toString), RunId(idTitle.getOrElse("") + entry.experiment))
      // we assume that PSM is always rank 1
      val matchInfo = PepMatchInfo(
        score = IdentScore(entry.score, Map()),
        numMissedCleavages = entry.missedCleavages,
        correctedMoz = entry.correctedMoz,
        correctedMolMass = entry.correctedMolMass,
        massDiff = entry.massDiff,
        massDiffUnit = Some(PPM),
        rank=Some(1),
        totalNumIons = None,
        chargeState = entry.chargeState,
        isRejected = Some(false),
        isContaminant = entry.isContaminant,
        modificationProbabilities = if(entry.modificationProbabilities.isEmpty) None else Some(entry.modificationProbabilities),
        highestModifProbability = if(entry.highestModifProbability.isEmpty) None else Some(entry.highestModifProbability),
        modificationInfos = if(entry.modificationInfos.isEmpty) None else Some(entry.modificationInfos)
      )

      val leadingProteinRef = ProteinRef(AccessionCode(entry.ac), Set(), Some(sequenceSource))
      val pepEntry = peptidesHash(entry.pepId)

      val proteinList = Seq(ProteinMatch(leadingProteinRef, pepEntry.previousAA, pepEntry.nextAA, pepEntry.startPos, pepEntry.endPos, pepEntry.isDecoy))

      val searchId = if(idTitle.isDefined) idTitle.get  + entry.experiment else entry.experiment
      Tuple2(RunId(entry.experiment), PepSpectraMatch(SearchId(searchId), spectrumId, pep, matchInfo, proteinList))
    })

    // Group the list by RunId to create a Map[RunId, List[RunId,PepSpectraMatch]] and then mapValues to create Map[RunId,List[PepSpectraMatch]]
    runIdToPepSpectraMatchList.groupBy(_._1).mapValues(list => list.map(_._2))
  }


  /**
   * parse a maxquant file and return search information.
   * @param maxQuantDir
   * @return
   */
  def parseSearchInfo(maxQuantDir: String, sequenceSource:SequenceSource, idTitle: Option[String]): Map[RunId, SearchInfo] = {

    // load files
    val file_params = new File(maxQuantDir + filename_params)
    val file_summary = new File(maxQuantDir + filename_summary)

    val paramsHash = parseMaxquantParametersTable(file_params)
    val summaryHash: Map[String, (String, String)] = parseMaxquantSummaryTable(file_summary)

    val username = paramsHash("User name")
    val parentTolerance = None
    val fragmentTolerance = paramsHash("MS/MS tol. (FTMS)")

    val searchInfoHashAux: Map[RunId, SearchInfo] = summaryHash.map({
      val nowDate = Calendar.getInstance().getTime()
      keyVal =>
        val searchId = if(idTitle.isDefined) idTitle.get + keyVal._1 else keyVal._1
        Tuple2(
        RunId(keyVal._1),
        SearchInfo(searchId=SearchId(searchId),
          title=keyVal._2._2,
          database=Seq(SearchDatabase(sequenceSource.value, None, None)),
          username=username,
          enzyme=keyVal._2._1,
          parentTolerance=parentTolerance,
          fragmentTolerance=fragmentTolerance,
          status = new SubmissionStatus("loading", "new SearchId was created"),
          creationDate=nowDate,
          searchEngine = Some("MaxQuant"))
        )
    })
    val searchInfoHash= searchInfoHashAux.filter({entry=> entry._1.toString !="RunId()"})
    searchInfoHash
  }


  def parse(maxQuantDir: String, idTitle: Option[String]): Seq[(Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo)] = {

    val file_params = new File(maxQuantDir + filename_params)

    //From parameters.txt
    val source = parseMaxquantParametersTable(file_params)("Fasta file")
    val sequenceSource = SequenceSource(parseFastaFilename(source))

    //parse summary.txt to obtain List(RunId)
    val runIdsAndRawfiles: Seq[(RunId, String)] = LoaderMaxQuant.getRunIds(new File(maxQuantDir + filename_summary))
    val runIds = runIdsAndRawfiles.map(_._1)

    val psmList: Map[RunId,Seq[PepSpectraMatch]] = loadPepSpectraMatch(maxQuantDir, runIds, sequenceSource, idTitle)
    val proteinList: Map[RunId, Seq[ProteinIdent]] = loadProtIdent(maxQuantDir, runIdsAndRawfiles, sequenceSource, idTitle)
    val searchInfo: Map[RunId, SearchInfo] = parseSearchInfo(maxQuantDir, sequenceSource, idTitle)

    val tupleSeq= runIds.map({
      runId =>  Tuple3(psmList(runId), proteinList(runId), searchInfo(runId))
    })
    tupleSeq
  }


  def parseZip(maxQuantZip: File, idTitle: Option[String]): Seq[(Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo)] = {
    val tmpDir = Unzip.unzip(maxQuantZip)

    val dirsInTmpDir = FileFinder.getListOfDirs(tmpDir)
    val txtDir = if(dirsInTmpDir.length == 1 && dirsInTmpDir(0).getName == "txt") tmpDir + "/txt/" else tmpDir + "/"

    parse(txtDir, idTitle)

  }


}

