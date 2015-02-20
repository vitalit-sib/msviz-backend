package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.{File, FileInputStream, InputStream}

import ch.isbsib.proteomics.mzviz.commons.helpers.OptionConverter
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.modifications.{ModifSource, ModifAC}
import ch.isbsib.proteomics.mzviz.modifications.models.{ModificationRef, PositionedModif, Modification}
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, NumDatabaseSequences, SequenceSource}
import org.apache.commons.io.FilenameUtils
import org.expasy.mzjava.proteomics.io.ms.ident.{MzIdentMlReader, PSMReaderCallback}
import org.expasy.mzjava.proteomics.mol.modification
import org.expasy.mzjava.proteomics.mol.modification.{ModificationList, ModificationResolver, ModAttachment}
import org.expasy.mzjava.proteomics.ms.ident.{ModificationMatch, PeptideMatch, PeptideProteinMatch, SpectrumIdentifier}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object LoaderMzIdent {

  /**
   * parse a .mzid file and return a full run.
   * @param file an .mzid file
   * @return
   */
  def parse(file: File, searchId: SearchId, runId: RunId): Seq[PepSpectraMatch] = {
    // data from MzJava parser are stored in a list
    val searchResults = mzJavaParse(file)

    // get the info about the SearchDatabases
    val searchDbSourceInfo = parseSearchDbSourceInfo(file)
    
    // convert the resulting list into our proper object
    searchResults.map({ t =>
      PepSpectraMatch(
        searchId = searchId,
        spectrumId = SpectrumId(
          SpectrumUniqueId(t._1.getSpectrum),
        runId = runId),
        pep = convertPeptide(t._2),
        matchInfo = convertPepMatch(t._2),
        proteinList = convertProtMatches(t._2, searchDbSourceInfo))
    }).toSeq

  }

  /**
   * parse the spectraFileName from the MzIdenML file. We do this seperately, since the MzJava parser doesn't take care of this information.
   * TODO: adapt MzJava MzIdentMlParser, so that it parses spectra filename information
   * @param filename MzIdentML path
   * @return spectra file name (e.g. blabla.mgf)
   */
  def parseSpectraFilename(filename: String): String = {
    val mzIdentML = scala.xml.XML.loadFile(filename)
    val spectraDataLocation = mzIdentML \\ "SpectraData" \ "@location"
    FilenameUtils.getBaseName(spectraDataLocation.text)
  }

  /**
   * parse the database  from the MzIdenML file. We do this seperately, since the MzJava parser doesn't take care of this information.
   * TODO: adapt MzJava MzIdentMlParser, so that it parses searchDb information
   * @param file MzIdentML file
   * @return a list of Tuples containing the SequenceSource and the number of entries
   */
  def parseSearchDbSourceInfo(file: File): Map[String, Tuple2[SequenceSource, NumDatabaseSequences]] = {
    val mzIdentML = scala.xml.XML.loadFile(file)

    (mzIdentML \\ "SearchDatabase").map { db =>
      ((db \ "@id").text -> Tuple2( SequenceSource((db \ "@version").text), NumDatabaseSequences((db \ "@numDatabaseSequences").text.toInt) ))
    }.toMap
  }


  /**
   * extract all the protein matches and convert it to our ProteinMatch class
   * @param mzJavaMatch a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertPeptide(mzJavaMatch: PeptideMatch): Peptide = {
    val pep = mzJavaMatch.toPeptide()
    Peptide(sequence = pep.toSymbolString, molMass = pep.getMolecularMass, modifications = convertModificationList(mzJavaMatch))
  }


  /**
   * extract all the protein matches and convert it to our ProteinMatch class
   * @param mzJavaMatch a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertProtMatches(mzJavaMatch: PeptideMatch, searchDbSourceInfo: Map[String, Tuple2[SequenceSource, NumDatabaseSequences]]): Seq[ProteinMatch] = {
    (for {
      pMatch: PeptideProteinMatch <- mzJavaMatch.getProteinMatches.iterator().asScala
    } yield {

      // match MzJava HitType to our own
      val isDecoy = mzJavaMatch.getProteinMatches.get(0).getHitType match {
        case PeptideProteinMatch.HitType.DECOY => Some(true)
        case PeptideProteinMatch.HitType.TARGET => Some(false)
        case _ => None
      }

      val searchDb = searchDbSourceInfo(pMatch.getSearchDatabase.get())._1
      ProteinMatch(proteinRef = ProteinRef(AC = AccessionCode(pMatch.getAccession),
        source = Some(searchDb)),
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
   * @param mzJavaMatch a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertPepMatch(mzJavaMatch: PeptideMatch): PepMatchInfo = {
    // create the score map
    val scoreMap:Map[String, Double] =
      (for {k <- mzJavaMatch.getScoreMap.keys()}
      yield {
        val key = k.asInstanceOf[String]
        key -> mzJavaMatch.getScoreMap.get(key)
      }).toMap

    // create and return a new PepMatchInfo
    PepMatchInfo(scoreMap = scoreMap,
      numMissedCleavages = Option(mzJavaMatch.getNumMissedCleavages),
      massDiff = Option(mzJavaMatch.getNumMissedCleavages),
      rank = mzJavaMatch.getRank,
      totalNumIons = Option(mzJavaMatch.getTotalNumIons),
      isRejected = Option(mzJavaMatch.isRejected))

  }

  /**
   * convert modification list from MzJava to MsViz
   * @param pep an .mzid file
   * @return
   */
  def convertModificationList(pep: PeptideMatch): Vector[Seq[Modification]] = {

    // get all modifications from MzJava
    val modifsAll = pep.getModifications(ModAttachment.all)

    // create list of positioned modifications
    val modifs:Seq[PositionedModif] = modifsAll.asScala.flatMap(convertMzModif(_))

    // create a vector with a list of modifs for each position
    modifs.foldLeft(Vector.fill[Seq[Modification]](pep.toSymbolString.length)(Nil))({
      (acc: Vector[Seq[Modification]], posMods) => acc.updated(posMods.pos, acc(posMods.pos):+posMods.modif)
    })

  }

  /**
   * convert a MzJava modification to our Modification
   * @param modif a MzJava ModificationMatch
   * @return a Modification
   */
  def convertMzModif(modif: ModificationMatch): Seq[PositionedModif] = {

    // adapt the position
    val pos:Int = modif.getModAttachment.name match {
      case "NTerm" => -1
      case "CTerm" => modif.getPosition + 1
      case _ => modif.getPosition
    }

    // get the postioned modifications
    val candidates:Seq[PositionedModif] = (0 to modif.getCandidateCount-1).toList.map({ i =>
      PositionedModif(modif = Modification(modifRef = ModificationRef(AC = ModifAC(modif.getModificationCandidate(i).getLabel), modifSource = ModifSource("TODO")), name="TODO", monoDeltaMass = 9999.99), pos = pos)
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
      def resultRead(identifier: SpectrumIdentifier, peptideMatch: PeptideMatch) = searchResults.append(Tuple2(identifier, peptideMatch))
    }

    val fr: InputStream = new FileInputStream(file)
    val reader: MzIdentMlReader = new MzIdentMlReader()

    reader.parse(fr, insertIdResultCB)
    searchResults
  }


}
