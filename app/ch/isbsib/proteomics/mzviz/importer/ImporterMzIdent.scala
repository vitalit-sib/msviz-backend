package ch.isbsib.proteomics.mzviz.importer

import java.io.{FileInputStream, InputStream}
import java.util

import org.expasy.mzjava.proteomics.io.ms.ident.{PSMReaderCallback, MzIdentMlReader}
import org.expasy.mzjava.proteomics.ms.ident.{SpectrumIdentifier, PeptideMatch}

import scala.collection.mutable.ListBuffer

/**
 * @author Roman Mylonas & Alexandre Masselot
 */
object ImporterMzIdent {

  /**
   * parse a file and return a full run.
   * Well, for the moment, it does not much :)
   * @param filename an .mzid file
   * @return
   */
  def parse(filename: String) = {
    val searchResults = ListBuffer[Tuple2[SpectrumIdentifier, PeptideMatch]]();

    val insertIdResultCB: PSMReaderCallback = new PSMReaderCallback {
      def resultRead(identifier: SpectrumIdentifier, peptideMatch: PeptideMatch) = searchResults.append(Tuple2(identifier, peptideMatch))
    }

    val fr: InputStream = new FileInputStream(filename)
    val reader: MzIdentMlReader = new MzIdentMlReader

    reader.parse(fr, insertIdResultCB)
    val searchResultMap  = searchResults.groupBy(_._1)
    for {(spId, list) <- searchResultMap} {
      println(spId, list)
    }
  }
}
