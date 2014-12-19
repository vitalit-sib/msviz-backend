package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.{FileInputStream, InputStream}

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, numDatabaseSequences, SequenceSource}
import org.apache.commons.io.FilenameUtils
import org.expasy.mzjava.proteomics.io.ms.ident.{MzIdentMlReader, PSMReaderCallback}
import org.expasy.mzjava.proteomics.ms.ident.{PeptideProteinMatch, SpectrumIdentifier, PeptideMatch}
import ch.isbsib.proteomics.mzviz.commons.{SpectraId, SpectraSource}
import scala.collection.JavaConverters._

import scala.collection.mutable.ListBuffer

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
object LoaderMzIdent {

  /**
   * parse a .mzid file and return a full run.
   * @param filename an .mzid file
   * @return
   */
  def parse(filename: String, searchId: SearchId): Seq[PepSpectraMatch] = {
    // data from MzJava parser are stored in a list
    val searchResults = mzJavaParse(filename)

    // this information should ideally be parsed by the MzJava parser
    val spectraFileName = parseSpectraFilename(filename)

    // convert the resulting list into our proper object
    searchResults.map(t => PepSpectraMatch(searchId = searchId, spId = SpectraId(t._1.getSpectrum),
      spSource = SpectraSource(spectraFileName),
      pep = convertPeptide(t._2),
      matchInfo = convertPepMatch(t._2),
      proteinList = convertProtMatches(t._2))
    ).toSeq

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
   * @param filename MzIdentML path
   * @return a list of Tuples containing the SequenceSource and the number of entries
   */
  def parseSearchDbSourceInfo(filename: String): Seq[Tuple2[SequenceSource, numDatabaseSequences]] = {
    val mzIdentML = scala.xml.XML.loadFile(filename)

    (mzIdentML \\ "SearchDatabase").map { db =>
      Tuple2( SequenceSource((db \ "@version").text), numDatabaseSequences((db \ "@numDatabaseSequences").text.toInt) )
    }
  }


  /**
   * extract all the protein matches and convert it to our ProteinMatch class
   * @param mzJavaMatch a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertPeptide(mzJavaMatch: PeptideMatch): Peptide = {
    val pep = mzJavaMatch.toPeptide
    Peptide(sequence = pep.toSymbolString, molMass = pep.getMolecularMass, dbSequenceRef = "TODO")
  }


  /**
   * extract all the protein matches and convert it to our ProteinMatch class
   * @param mzJavaMatch a PeptideMatch obtained from the MzJava mzIdentML parser
   * @return
   */
  def convertProtMatches(mzJavaMatch: PeptideMatch): Seq[ProteinMatch] = {
    (for {
      pMatch: PeptideProteinMatch <- mzJavaMatch.getProteinMatches.iterator().asScala
    } yield {
      ProteinMatch(proteinRef = ProteinRef(AC = AccessionCode(pMatch.getAccession), source = SequenceSource("TODO")), previousAA = pMatch.getPreviousAA, nextAA = pMatch.getNextAA, startPos = pMatch.getStart, endPos = pMatch.getEnd)
    }) toSeq
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
        (key -> mzJavaMatch.getScoreMap.get(key))
      }) toMap

    // create and return a new PepMatchInfo
    PepMatchInfo(scoreMap = scoreMap,
      numMissedCleavages = Option(mzJavaMatch.getNumMissedCleavages),
      massDiff = Option(mzJavaMatch.getNumMissedCleavages),
      rank = mzJavaMatch.getRank,
      totalNumIons = Option(mzJavaMatch.getTotalNumIons),
      // modifications
      // precursor neutral mass
      // isDecoy = Option(mzJavaMatch)
      isRejected = Option(mzJavaMatch.isRejected))

  }


  /**
   * parse .mzid file using MzIdentMlReader from MzJava
   * @param filename an .mzid file
   * @return
   */
  def mzJavaParse(filename: String): ListBuffer[Tuple2[SpectrumIdentifier, PeptideMatch]] = {
    val searchResults = ListBuffer[Tuple2[SpectrumIdentifier, PeptideMatch]]()

    val insertIdResultCB: PSMReaderCallback = new PSMReaderCallback {
      def resultRead(identifier: SpectrumIdentifier, peptideMatch: PeptideMatch) = searchResults.append(Tuple2(identifier, peptideMatch))
    }

    val fr: InputStream = new FileInputStream(filename)
    val reader: MzIdentMlReader = new MzIdentMlReader()

    reader.parse(fr, insertIdResultCB)

    searchResults
  }


}
