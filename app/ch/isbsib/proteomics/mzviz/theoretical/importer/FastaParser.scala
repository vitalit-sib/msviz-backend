package ch.isbsib.proteomics.mzviz.theoretical.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry

import scala.util.matching.Regex

/**
 * opens a file and get a list of fasta entries
 *
 * @author Trinidad Martín
 */
class FastaParser(file: File) {
  val reHeader = """>?..\|(.*?)\|.*""".r

  def parseOneProtBlock(protLines: String): FastaEntry = {
    val firstNewLineIndex = protLines.indexOf("\n")
    val headline = protLines.substring(0, firstNewLineIndex)
    val seqLines = protLines.substring(firstNewLineIndex + 1)

    //gett accession code and cleanup sequence
    val reHeader(ac) = headline
    val seq = seqLines.replaceAll( """\s+""", "")

    FastaEntry(AccessionCode(ac), seq)
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
  def apply(filename: String) = new FastaParser(new File(filename))
}