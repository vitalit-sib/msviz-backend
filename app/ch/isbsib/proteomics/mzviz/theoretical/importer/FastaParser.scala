package ch.isbsib.proteomics.mzviz.theoretical.importer

import java.io.File
import java.util.Scanner

import ch.isbsib.proteomics.mzviz.matches.models.ProteinRef
import ch.isbsib.proteomics.mzviz.theoretical.{ProteinIdentifier, SequenceSource, AccessionCode}
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService

import scala.util.matching.Regex

/**
 * opens a file and get a list of FAsta entries
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class FastaParser(file: File, source: SequenceSource, regexp:Option[String]) {


  def parseOneProtBlock(protLines: String): FastaEntry = {
    val firstNewLineIndex = protLines.indexOf("\n")
    val headline = protLines.substring(0, firstNewLineIndex)
    val seqLines = protLines.substring(firstNewLineIndex + 1)

    //get accession code and cleanup sequence
    val ac = FastaExtractorACFromHeader.parseAC(headline, regexp)
    val ids = FastaExtractorACFromHeader.parseIdentifiers(headline) + ProteinIdentifier(ac.value)
    val seq = seqLines.replaceAll( """\s+""", "")

    FastaEntry(ProteinRef(ac, ids, Some(source)), seq, seq.size)
    //val=SequenceMongoDBService()
  }

  /**
   * parse the given source and produce an iterator of FastaEntry
   * @return
   */
  def parse: Iterator[FastaEntry] = {

    val scanner = new Scanner(file).useDelimiter( """\n>""")

    //we build an iterator over the file
    val it: Iterator[String] = new Iterator[String] {
      override def hasNext: Boolean = scanner.hasNext

      override def next(): String = scanner.next()
    }

    it.map(parseOneProtBlock)

  }
}


/**
 * companion object
 */
object FastaParser {

  def apply(filename: String, source: SequenceSource, regexp:Option[String]) = new FastaParser(new File(filename), source, regexp)

  def apply(file: File, source: SequenceSource, regexp:Option[String]) = new FastaParser(file, source, regexp)

}

object FastaExtractorACFromHeader {
  val reACList = List(
    """..\|(.+?)\|.*""",
    """..\|([\w\-]+).*""",
    """([\w\-:]+).*"""
  ).map(s => ("^>?" + s).r)

  val reIdentifiersList = List(
    """..\|.+?\|(\S+)\s*.*"""
  ).map(s => ("^>?" + s).r)

  def parseAC(header: String, regexp:Option[String]): AccessionCode = {
    val localReACList:List[Regex] = if(regexp.isDefined) regexp.get.split(",").map(_.r).toList else reACList
    localReACList.find(_.findFirstMatchIn(header).isDefined) match {
      case Some(re) =>
        val re(ac) = header
        AccessionCode(ac)
      case None => throw new FastaParsingException(s"cannot parse AC from header: $header")
    }
  }

  def parseIdentifiers(header: String): Set[ProteinIdentifier] = reIdentifiersList.find(_.findFirstMatchIn(header).isDefined) match {
    case Some(re) =>
      val re(ids )= header
      ids.split(",").toList.toSet.map( ProteinIdentifier.apply)
    case None => Set()
  }
}