package ch.isbsib.proteomics.mzviz.spectrasim.calcsim

import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpMatch
import org.expasy.mzjava.core.ms.AbsoluteTolerance
import org.expasy.mzjava.core.ms.peaklist.{DoublePeakList, PeakAnnotation, PeakList}
import org.expasy.mzjava.core.ms.spectrasim.NdpSimFunc

/**
 * computes the normalized dot product between 2 spectra
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class NormDotProdSim extends SpectraSimilarity{

  def calcSimilarity(sp1: ExpMSnSpectrum, sp2: ExpMSnSpectrum, absTolerance: Double = 0.5): SpSpMatch ={

    // convert the peaklists
    val peakList1: PeakList[PeakAnnotation] = new DoublePeakList[PeakAnnotation]
    val peakList2: PeakList[PeakAnnotation] = new DoublePeakList[PeakAnnotation]

    def sortPeaks(sp: ExpMSnSpectrum) =  sp.peaks.sortBy(_.moz.value).map(p => Tuple2(p.moz.value, p.intensity.value))
    val peaks1 = sortPeaks(sp1)
    val peaks2 = sortPeaks(sp2)

    peakList1.addSorted(peaks1.map(_._1).toArray, peaks1.map(_._2).toArray)
    peakList2.addSorted(peaks2.map(_._1).toArray, peaks2.map(_._2).toArray)

    // compute the similarity
    val simFunc: NdpSimFunc[PeakAnnotation, PeakAnnotation] = new NdpSimFunc[PeakAnnotation, PeakAnnotation](0, new AbsoluteTolerance(absTolerance))
    val sim: Double = simFunc.calcSimilarity(peakList1, peakList2)

    SpSpMatch(sp1, sp2, sim)
  }

}
