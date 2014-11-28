package ch.isbsib.proteomics.mzviz.theoretical.importer

import ch.isbsib.proteomics.mzviz.theoretical.models.Fasta

import scala.io.Source
import java.io.{FileReader, FileNotFoundException, IOException}

/**
 * Load Fasta file
 * @author Trinidad Martín
 */
object LoaderFasta extends App {

  val filename = "/Users/tmartinc/M_100small.fasta"
  var seq=""
  var ac=""

  try
    for (line <- Source.fromFile(filename).getLines()) {
      //Parse ac and seq
      val patternAc = """\|(.*)\|""".r
      if(patternAc.findFirstIn(line).isDefined){
        if (!seq.equals("")){
          //Create Fasta object
          val fastaObject = new Fasta(ac, seq)
          seq=""
        }
        val words=line.split("""\|""")
        ac=words(1)
        }
      else {
        seq+=line
        }

    }
  catch {
    case ex: FileNotFoundException => println("Couldn't find that file.")
    case ex: IOException => println("Had an IOException trying to read that file")
  }
  //Create last Fasta object
  val fastaObject = new Fasta(ac, seq)

}
