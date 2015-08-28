package ch.isbsib.proteomics.mzviz.experimental.importer

import java.io.File
import java.util


import ch.isbsib.proteomics.mzviz.commons.{Moz, Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpPeakMS1, SpectrumId, ExpMs1Spectrum}
import org.expasy.mzjava.core.io.ms.spectrum.MzxmlReader
import org.expasy.mzjava.core.io.ms.spectrum.MzxmlReader.ConsistencyCheck
import org.expasy.mzjava.core.ms.peaklist.PeakList
import org.expasy.mzjava.core.ms.spectrum.MsnSpectrum

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Load an MzXML file into an MSRun
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object LoaderMzXML {

  def parseFile(file: File, runId: RunId):Iterator[ExpMs1Spectrum] = {
    return new MzXmlIterator(file, runId)
  }

}

/**
 * produces an iterator over ExpMs1Spectra, derived from MzJava MsnSpectra
 * @param file mzXML to read from
 */
class MzXmlIterator(file: File, runId: RunId) extends Iterator[ExpMs1Spectrum] {

  val mzXmlReader = new MzxmlReader(file, PeakList.Precision.DOUBLE)

  // we have to accept a couple of exceptions
  // because of the peak 445.12 (polysolyxan)
  // in the MzXML it is considered as base peak but filtered out in peak values
  mzXmlReader.acceptUnsortedSpectra()
  mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.MOST_INTENSE_PEAK))
  mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.TOTAL_ION_CURRENT))
  mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.PEAKS_COUNT))
  mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.LOWEST_MZ))
  mzXmlReader.removeConsistencyChecks(util.EnumSet.of(ConsistencyCheck.HIGHEST_MZ))


  override def hasNext: Boolean = mzXmlReader.hasNext

  override def next(): ExpMs1Spectrum = if(hasNext){
    val ret = mzXmlReader.next
    msnSpectrum2ExpMs1Spectrum(ret)
  } else {
      throw new IllegalArgumentException("Cannot call next when not having next element")
    }

  def msnSpectrum2ExpMs1Spectrum(sp: MsnSpectrum): ExpMs1Spectrum = {
    val spId = new SpectrumId(SpectrumUniqueId(sp.getScanNumbers.getFirst.getValue.toString), runId)
    val rt = new RetentionTime(sp.getRetentionTimes.getFirst.getTime)
    val peaks = sp.getMzs(null).zip(sp.getIntensities(null)).map { case (mz, int) => ExpPeakMS1(Moz(mz), Intensity(int)) }

    new ExpMs1Spectrum(spId, rt, peaks.toList)
  }

}


