package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.{File, FileInputStream, InputStream}
import java.util.Calendar

import ch.isbsib.proteomics.mzviz.commons.Dalton
import ch.isbsib.proteomics.mzviz.commons.helpers.OptionConverter
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.{HitRank, SearchId}
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.modifications.{ModifName}
import ch.isbsib.proteomics.mzviz.modifications.models.{PositionedModifRef}
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, NumDatabaseSequences, SequenceSource}
import org.apache.commons.io.FilenameUtils
import org.expasy.mzjava.proteomics.io.ms.ident.{MzIdentMlReader, PSMReaderCallback}
import org.expasy.mzjava.proteomics.mol.modification.ModAttachment
import org.expasy.mzjava.proteomics.mol.modification.unimod.UnimodManager
import org.expasy.mzjava.proteomics.ms.ident.{ModificationMatch, PeptideMatch, PeptideProteinMatch, SpectrumIdentifier}
import play.{Logger, Play}
import java.nio.file.{Paths, Files}
import com.google.common.base.Optional

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.xml.Elem

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object LoaderMzIdent {


  def parseWithXmlElem(file: File, searchId: SearchId, runId: RunId, mzidXml: Elem, searchEngine: Option[String]): Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = {

    // we try to take the unimod information from Play application if it is started
    if(play.api.Play.maybeApplication.isDefined) {

      val confMap = Play.application().configuration().asMap()

      val unimodConfigTag = "unimod"

      if (confMap.containsKey(unimodConfigTag)) {
        val unimodPath = confMap.get(unimodConfigTag).toString

        if (Files.exists(Paths.get(unimodPath))) {
          UnimodManager.setUnimodPath(unimodPath)
        } else {
          Logger.warn("could not load UniMod file from :[" + unimodPath + "]")
        }
      }
    }

    // get the info about the SearchDatabases
    val searchDbSourceInfo = parseSearchDbSourceInfo(mzidXml)

    // parse PSM, Protein matches and searchInfo
    val psmList = parsePsm(file, searchId, runId, searchDbSourceInfo)
    val proteinList = ParseProteinMatches.parseProtList(mzidXml, searchId, searchDbSourceInfo)
    val searchInfo = parseSearchInfo(mzidXml, searchId, searchEngine)

    Tuple3(psmList, proteinList, searchInfo)

  }

  /**
   * parse the mzIdentML file
   *
   * @param file
   * @param searchId
   * @param runId
   * @return
   */
  def parse(file: File, searchId: SearchId, runId: RunId, searchEngine: Option[String]): (Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo) = {
    // load MzIdentML as scala xml elem
    val mzidXml = scala.xml.XML.loadFile(file)

    this.parseWithXmlElem(file, searchId, runId, mzidXml, searchEngine)
  }


  /**
   * parse a .mzid file and return a full run.
   * @param file an .mzid file
   * @return
   */
  def parsePsm(file: File, searchId: SearchId, runId: RunId, searchDbSourceInfo:Seq[SearchDatabase]): Seq[PepSpectraMatch] = {

    // data from MzJava parser are stored in a list
    val searchResults:ListBuffer[(SpectrumIdentifier, PeptideMatch)] = mzJavaParse(file)

    // convert the resulting list into our proper object
    val psms = searchResults.map({ t =>

      val spectrumTitle:String= t._1.getSpectrum
      val reTitleScan = """.*\.(\d+)\.\d$""".r
      val scanNumber = spectrumTitle match {
          case reTitleScan(s) => s
          case _ => spectrumTitle
        }
      PepSpectraMatch(
        searchId = searchId,
        spectrumId = SpectrumId(

        //Not sure if index is not corresponding to scanNumber
         SpectrumUniqueId(scanNumber),
          runId = runId),
        pep = convertPeptide(t._2),
        matchInfo = convertPepMatch(t),
        proteinList = convertProtMatches(t._2, searchDbSourceInfo))
    })

    // entries that have the exact same score as the preceding entry are considered to have the same rank
    val psmsWithExtraFirst:Seq[PepSpectraMatch] = psms(0)+:psms
    psmsWithExtraFirst.sliding(2,1).map({ psmsPair =>
     val (p1:PepSpectraMatch, p2:PepSpectraMatch) = (psmsPair(0), psmsPair(1))
      if(p1.spectrumId == p2.spectrumId && p1.matchInfo.score.mainScore == p2.matchInfo.score.mainScore){
        val newMatchInfo = p2.matchInfo.copy(rank=p1.matchInfo.rank)
        p2.copy(matchInfo = newMatchInfo)
      }else{
        p2
      }
    }).toSeq

  }
  /**
   * parse a .mzid file and return search information.
   * @param mzidXml Scala XML element
   * @return
   */
  def parseSearchInfo(mzidXml: Elem, searchId: SearchId, searchEngine: Option[String]): SearchInfo = {

    // get the info about the SearchDatabases
    val title = parseTitleFilename(mzidXml)
    val database = parseSearchDbSourceInfo(mzidXml)
    val username = parseUsernameFilename(mzidXml)
    val enzyme = parseEnzymeFilename(mzidXml)
    val parentTolerance = Option(parseParentToleranceFilename(mzidXml))
    val fragmentTolerance = parseFragmentToleranceFilename(mzidXml)
    val nowDate = Calendar.getInstance().getTime()

    SearchInfo(
      searchId,
      title,
      database,
      username,
      enzyme,
      parentTolerance,
      fragmentTolerance,
      new SubmissionStatus ("loading", "created new SearchInfo"),
      nowDate,
      searchEngine)
  }

  /**
   * parse the spectraFileName from the MzIdenML file. We do this seperately, since the MzJava parser doesn't take care of this information.
   * TODO: adapt MzJava MzIdentMlParser, so that it parses spectra filename information
   * @param mzidXml Scala XML element
   * @return spectra file name (e.g. blabla.mgf)
   */
  def parseSpectraFilename(mzidXml: Elem): String = {
    val spectraDataLocation = mzidXml \\ "SpectraData" \ "@location"
    FilenameUtils.getBaseName(spectraDataLocation.text)

  }

  /**
   * parse the title from the MzIdenML file.
   * @param mzidXml Scala XML element
   * @return title file name
   */
  def parseTitleFilename(mzidXml: Elem): String = {
    // to have only the first of the values
    val titleLocation = (mzidXml \\ "userParam" \\ "@value").head
    FilenameUtils.getBaseName(titleLocation.text)
  }

  /**
   * parse the username from the MzIdenML file.
   * @param mzidXml Scala XML element
   * @return username
   */
  def parseUsernameFilename(mzidXml: Elem): String = {
    val usernameLocation =mzidXml \\ "Person" \\ "@name"
    FilenameUtils.getBaseName(usernameLocation.text)
  }

  /**
   * parse the enzyme from the MzIdenML file.
   * @param mzidXml Scala XML element
   * @return enzyme
   */
  def parseEnzymeFilename(mzidXml: Elem): String = {
    val enzymeLocation =mzidXml \\ "EnzymeName" \ "cvParam" \ "@name"
    if(enzymeLocation.size == 0){
      (mzidXml \\ "EnzymeName" \ "userParam" \ "@value").text
    }else{
      enzymeLocation.text
    }
  }

  /**
   * parse the parent tolerance from the MzIdenML file.
   * @param mzidXml Scala XML element
   * @return parent tolerance
   */
  def parseParentToleranceFilename(mzidXml: Elem): String = {
    val toleranceValue=((mzidXml \\ "ParentTolerance" \ "cvParam").filter(n =>(n \ "@name").text == "search tolerance plus value") \ "@value").text
    val toleranceUnitValue=((mzidXml \\ "ParentTolerance" \ "cvParam").filter(n =>(n \ "@name").text == "search tolerance plus value") \ "@unitName").text

    val tolerance=toleranceValue + " " + toleranceUnitValue
    tolerance
  }

  /**
   * parse the fragment tolerance from the MzIdenML file.
   * @param mzidXml Scala XML element
   * @return fragment tolerance
   */
  def parseFragmentToleranceFilename(mzidXml: Elem): String = {
    val toleranceValue=((mzidXml \\ "FragmentTolerance" \ "cvParam").filter(n =>(n \ "@name").text == "search tolerance plus value") \ "@value").text
    val toleranceUnitValue=((mzidXml \\ "FragmentTolerance" \ "cvParam").filter(n =>(n \ "@name").text == "search tolerance plus value") \ "@unitName").text

    val tolerance=toleranceValue + " " + toleranceUnitValue
    tolerance
  }

  /**
   * parse the database  from the MzIdenML file. We do this seperately, since the MzJava parser doesn't take care of this information.
   * TODO: adapt MzJava MzIdentMlParser, so that it parses searchDb information
   * @param mzidXml Scala XML elem
   * @return a list of Tuples containing the SequenceSource and the number of entries
   */
  def parseSearchDbSourceInfo(mzidXml: Elem):Seq[SearchDatabase] = {
    (mzidXml \\ "SearchDatabase").map { db =>
      SearchDatabase((db \ "@id").text,Some(((db \ "@version").text)),Some(((db \ "@numDatabaseSequences").text.toInt)))
    }
  }


  /**
   * extract all the protein matches and convert it to our ProteinMatch class
   * @param mzJavaMatch a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertPeptide(mzJavaMatch: PeptideMatch): Peptide = {
    val pep = mzJavaMatch.toPeptide()

    // since MzJava can throw an Exception on that, we'll wrap it around a try catch
    var mMass:Option[Double] = None
    try{
      mMass = Option(pep.getMolecularMass)
    }catch{
      case e: Exception => Logger.warn(e.getMessage)
    }

    Peptide(sequence = pep.toSymbolString, molMass = mMass, modificationNames = convertModificationList(mzJavaMatch))
  }


  /**
   * extract all the protein matches and convert it to our ProteinMatch class
   * @param mzJavaMatch a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertProtMatches(mzJavaMatch: PeptideMatch, searchDbSourceInfo: Seq[SearchDatabase]): Seq[ProteinMatch] = {
    (for {
      pMatch: PeptideProteinMatch <- mzJavaMatch.getProteinMatches.iterator().asScala
    } yield {

      // match MzJava HitType to our own
      val isDecoy = mzJavaMatch.getProteinMatches.get(0).getHitType match {
        case PeptideProteinMatch.HitType.DECOY => Some(true)
        case PeptideProteinMatch.HitType.TARGET => Some(false)
        case _ => None
      }

      val searchDb = searchDbSourceInfo.find(db =>
        db.id==pMatch.getSearchDatabase.get()).map(db =>
          SequenceSource(db.version.get)
      )

      ProteinMatch(proteinRef = ProteinRef(AC = AccessionCode(pMatch.getAccession),
        source = searchDb),
        previousAA = OptionConverter.convertGoogleOption(pMatch.getPreviousAA),
        nextAA = OptionConverter.convertGoogleOption(pMatch.getNextAA),
        startPos = pMatch.getStart,
        endPos = pMatch.getEnd,
        isDecoy = isDecoy
      )
    }).toSeq
  }

  /**
   * convert a MzJava PeptideMatch into our PepMatchInfo object
   * @param mzJavaRes a Tuple of a SpectrumIdentifier and a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertPepMatch(mzJavaRes: (SpectrumIdentifier, PeptideMatch)): PepMatchInfo = {
    val mzJavaMatch = mzJavaRes._2

    val MainScoreName = "Mascot:score"

    // create the score map
    val scoreMap:Map[String, Double] =
      (for {k <- mzJavaMatch.getScoreMap.keys()}
      yield {
        val key = k.asInstanceOf[String]
        key -> mzJavaMatch.getScoreMap.get(key)
      }).toMap

    val identScore = IdentScore(scoreMap.get(MainScoreName).getOrElse(-1.0), scoreMap)

    // create and return a new PepMatchInfo
    PepMatchInfo(
      score = identScore,
      numMissedCleavages = OptionConverter.convertGoogleOption[Int](mzJavaMatch.getNumMissedCleavages.asInstanceOf[Optional[Int]]),
      correctedMoz = None,
      correctedMolMass = None,
      massDiff = OptionConverter.convertGoogleOption[Double](mzJavaMatch.getMassDiff.asInstanceOf[Optional[Double]]),
      massDiffUnit = Some(Dalton),
      rank = OptionConverter.convertGoogleOption[Int](mzJavaMatch.getRank.asInstanceOf[Optional[Int]]),
      chargeState = OptionConverter.convertGoogleOption[Int](mzJavaRes._1.getAssumedCharge.asInstanceOf[Optional[Int]]),
      totalNumIons = OptionConverter.convertGoogleOption[Int](mzJavaMatch.getTotalNumIons.asInstanceOf[Optional[Int]]),
      isRejected = OptionConverter.convertGoogleOption[Boolean](mzJavaMatch.isRejected.asInstanceOf[Optional[Boolean]]),
      isContaminant = None,
      modificationProbabilities = None,
      highestModifProbability = None,
      modificationInfos = None
    )

  }

  /**
   * convert modification list from MzJava to MsViz
   * @param pep an .mzid file
   * @return
   */
  def convertModificationList(pep: PeptideMatch): Vector[Seq[ModifName]] = {

    // get all modifications from MzJava
    val modifsAll = pep.getModifications(ModAttachment.all)

    // create list of positioned modifications
    val modifs:Seq[PositionedModifRef] = modifsAll.asScala.flatMap(convertMzModif(_))

    // create a vector with a list of modifs for each position (+2 because we add N and C-term modifications)
    modifs.foldLeft(Vector.fill[Seq[ModifName]](pep.toSymbolString.length + 2)(Nil))({
      (acc: Vector[Seq[ModifName]], posMods) => acc.updated(posMods.pos, acc(posMods.pos):+posMods.modifName)
    })

  }

  /**
   * convert a MzJava modification to our Modification
   * @param modif a MzJava ModificationMatch
   * @return a Modification
   */
  def convertMzModif(modif: ModificationMatch): Seq[PositionedModifRef] = {

    // adapt the position
    val pos:Int = modif.getModAttachment.name match {
      case "N_TERM" => 0
      case "C_TERM" => modif.getPosition + 2
      case _ => modif.getPosition + 1
    }

    // get the postioned modifications
    val candidates:Seq[PositionedModifRef] = (0 to modif.getCandidateCount-1).toList.map({ i =>
      // @TODO source should be parsed from MzIdentML (adaptations in MzJava needed). Currently the source is UNIMOD obtained from Mascot server
      PositionedModifRef(modifName = ModifName(modif.getModificationCandidate(i).getLabel), pos = pos)
    })

    candidates

  }


    /**
   * parse .mzid file using MzIdentMlReader from MzJava
   * @param file an .mzid file
   * @return
   */
  def mzJavaParse(file: File): ListBuffer[Tuple2[SpectrumIdentifier, PeptideMatch]] = {
    val searchResults = ListBuffer[Tuple2[SpectrumIdentifier, PeptideMatch]]()
    val insertIdResultCB: PSMReaderCallback = new PSMReaderCallback {
      def resultRead(identifier: SpectrumIdentifier, peptideMatch: PeptideMatch) = {
        //@TODO store the SpectrumIdentifictionItem in MzJava as the name
        //@TODO MzJava provides an spectra index (is it the same as ScanNumber?)
        searchResults.append(Tuple2(identifier, peptideMatch))
      }
    }

    val fr: InputStream = new FileInputStream(file)
    val reader: MzIdentMlReader = new MzIdentMlReader()

    reader.parse(fr, insertIdResultCB)
    searchResults
  }


}
