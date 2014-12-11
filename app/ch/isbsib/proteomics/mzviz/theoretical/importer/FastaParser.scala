package ch.isbsib.proteomics.mzviz.theoretical.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService

import scala.util.matching.Regex

/**
 * opens a file and get a list of fasta entries
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
class FastaParser(file: File, source:Option[SequenceSource]) {
  val reHeader = """>?..\|(.*?)\|.*""".r

  def parseOneProtBlock(protLines: String): FastaEntry = {
    val firstNewLineIndex = protLines.indexOf("\n")
    val headline = protLines.substring(0, firstNewLineIndex)
    val seqLines = protLines.substring(firstNewLineIndex + 1)

    //gett accession code and cleanup sequence
    val reHeader(ac) = headline
    val seq = seqLines.replaceAll( """\s+""", "")

    FastaEntry(AccessionCode(ac), seq, source)
    //val=SequenceMongoDBService()
  }


  def parse: Seq[FastaEntry] = {
    //Get the whole text
    val lines = scala.io.Source.fromFile(file).mkString

    lines
      .split( """\n>""")
      .map(parseOneProtBlock)

  }
}


/**
 * companion object
 */
object FastaParser {
  def apply(filename: String, source:SequenceSource) = new FastaParser(new File(filename), Some(source))
  def apply(filename: String) = new FastaParser(new File(filename), None)
}