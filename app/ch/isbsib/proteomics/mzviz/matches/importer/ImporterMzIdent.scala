package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.{FileInputStream, InputStream}

import org.expasy.mzjava.proteomics.io.ms.ident.{MzIdentMlReader, PSMReaderCallback}
import org.expasy.mzjava.proteomics.ms.ident.{PeptideMatch, SpectrumIdentifier}

import scala.collection.mutable.ListBuffer

/**
 * @author Roman Mylonas & Alexandre Masselot
 */
object ImporterMzIdent {

  /**
   * parse a .mzid file and return a full run.
   * @param filename an .mzid file
   * @return
   */
  def parse(filename: String) = {
    // data from MzJava parser are stored in a list
    val searchResults = mzJavaParse(filename)

    // convert the resulting list into our proper objects
    val searchResultMap  = searchResults.groupBy(_._1)
    for {(spId, list) <- searchResultMap} {
      println(spId.getIndex, list)
    }
  }

  /**
   * parse .mzid file using MzIdentMlReader from MzJava
   * @param filename an .mzid file
   * @return
   */
  def mzJavaParse(filename: String): ListBuffer[Tuple2[SpectrumIdentifier, PeptideMatch]]= {
    val searchResults = ListBuffer[Tuple2[SpectrumIdentifier, PeptideMatch]]()

    val insertIdResultCB: PSMReaderCallback = new PSMReaderCallback {
      def resultRead(identifier: SpectrumIdentifier, peptideMatch: PeptideMatch) = searchResults.append(Tuple2(identifier, peptideMatch))
    }

    val fr: InputStream = new FileInputStream(filename)
    val reader: MzIdentMlReader = new MzIdentMlReader

    reader.parse(fr, insertIdResultCB)

    searchResults
  }





}
