package ch.isbsib.proteomics.mzviz.theoretical.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry

/**
 * opens a file and get a list of fasta entries
 *
 * @author Trinidad Martin
 */
class FastaParser(file:File) {
  def parse:Seq[FastaEntry] = Nil
}


/**
 * companion object
 */
object FastaParser {
  def apply(filename:String) = new FastaParser(new File(filename))
}