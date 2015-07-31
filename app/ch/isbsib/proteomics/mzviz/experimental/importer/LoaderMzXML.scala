package ch.isbsib.proteomics.mzviz.experimental.importer

import java.io.File
import java.util


import org.expasy.mzjava.core.io.ms.spectrum.MzxmlReader
import org.expasy.mzjava.core.io.ms.spectrum.MzxmlReader.ConsistencyCheck
import org.expasy.mzjava.core.ms.peaklist.PeakList
import org.expasy.mzjava.core.ms.spectrum.MsnSpectrum

import scala.collection.mutable.ListBuffer

/**
 * Load an MzXML file into an MSRun
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object LoaderMzXML {

  def parseFile(file: File):Seq[MsnSpectrum] = {

    val mzXmlReader = new MzxmlReader(file, PeakList.Precision.DOUBLE)
    mzXmlReader.acceptUnsortedSpectra()
    mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.MOST_INTENSE_PEAK))
    mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.TOTAL_ION_CURRENT))
    mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.PEAKS_COUNT))

    var spList = new ListBuffer[MsnSpectrum]()
    while(mzXmlReader.hasNext){
      spList += mzXmlReader.next
    }

    spList.toSeq
  }


}


