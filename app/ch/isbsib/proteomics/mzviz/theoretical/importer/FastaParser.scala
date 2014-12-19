package ch.isbsib.proteomics.mzviz.theoretical.importer

import java.io.File
import java.util.Scanner

import ch.isbsib.proteomics.mzviz.matches.models.ProteinRef
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService

import scala.util.matching.Regex

/**
 * opens a file and get a list of fasta entries
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, Swiss Institute of Bioinformatics
 */
class FastaParser(file: File, source: SequenceSource) {
  val reHeader = """>?..\|(.*?)\|.*""".r

  def parseOneProtBlock(protLines: String): FastaEntry = {
    val firstNewLineIndex = protLines.indexOf("\n")
    val headline = protLines.substring(0, firstNewLineIndex)
    val seqLines = protLines.substring(firstNewLineIndex + 1)

    //gett accession code and cleanup sequence
    val reHeader(ac) = headline
    val seq = seqLines.replaceAll( """\s+""", "")

    FastaEntry(ProteinRef(AccessionCode(ac), source), seq)
    //val=SequenceMongoDBService()
  }

  /**
   * parse the given source and produces and iterator of FastaEntry
   * @return
   */
  def parse: Iterator[FastaEntry] = {

    val scanner = new Scanner(file).useDelimiter( """\n>""");

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
  def apply(filename: String, source: SequenceSource) = new FastaParser(new File(filename), source)

}