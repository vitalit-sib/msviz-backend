package ch.isbsib.proteomics.mzviz.spectrasim.calcsim

import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpMatch
import org.expasy.mzjava.core.ms.AbsoluteTolerance
import org.expasy.mzjava.core.ms.peaklist.{DoublePeakList, PeakAnnotation, PeakList}
import org.expasy.mzjava.core.ms.spectrasim.NdpSimFunc
import org.expasy.mzjava.core.ms.spectrasim.peakpairprocessor.transformer.PeakPairIntensitySqrtTransformer

/**
 * computes the normalized dot product between 2 spectra or between a spectra and a list of spectra
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class NormDotProdSim extends SpectraSimilarity{

  def calcSimilarity(sp1: ExpMSnSpectrum, sp2: ExpMSnSpectrum, absTolerance: Double): SpSpMatch = {

    def peakList1 = convertSpectrum(sp1)
    def peakList2 = convertSpectrum(sp2)

    // compute the similarity
    val simFunc = createSimFunc(absTolerance)
    val sim: Double = simFunc.calcSimilarity(peakList1, peakList2)

    SpSpMatch(sp1, sp2, sim)
  }


  def calcSimilarityList(sp1: ExpMSnSpectrum, sp2List: Seq[ExpMSnSpectrum], absTolerance: Double): Seq[SpSpMatch] = {

    def peakList1 = convertSpectrum(sp1)

    // prepare the MzJava similarity
    val simFunc = createSimFunc(absTolerance)

    // compute SpSpMatches
    val spSpMatches = sp2List.map({ sp2 =>
      val peakList2 = convertSpectrum(sp2)
      val sim = simFunc.calcSimilarity(peakList1, peakList2)
      SpSpMatch(sp1, sp2, sim)
    })

    spSpMatches
  }

  /**
   * Convert msViz spectrum to MzJava peaklist
   * @param sp msViz spectrum
   * @return MzJava peaklist
   */
  def convertSpectrum(sp: ExpMSnSpectrum): PeakList[PeakAnnotation] = {
    val peakList: PeakList[PeakAnnotation] = new DoublePeakList[PeakAnnotation]

    def sortPeaks(sp: ExpMSnSpectrum) =  sp.peaks.sortBy(_.moz.value).map(p => Tuple2(p.moz.value, p.intensity.value))
    val peaks = sortPeaks(sp)

    peakList.addSorted(peaks.map(_._1).toArray, peaks.map(_._2).toArray)
    peakList
  }

  /**
   * NdpSimFunc<>(0, new DefaultPeakListAligner<>(fragmentTolerance), new PeakPairIntensitySqrtTransformer<>())
   *
   * create the MzJava similarity function using PeakPairIntensitySqrt transformation
   * @param absTolerance
   * @return
   */
  def createSimFunc(absTolerance: Double): NdpSimFunc[PeakAnnotation, PeakAnnotation] = {
    //new NdpSimFunc[PeakAnnotation, PeakAnnotation](0, new AbsoluteTolerance(absTolerance), new PeakPairIntensitySqrtTransformer[PeakAnnotation, PeakAnnotation]())
    new NdpSimFunc[PeakAnnotation, PeakAnnotation](0, new AbsoluteTolerance(absTolerance))
  }



}

/**
 * companion object
 */
object NormDotProdSim{
  def apply() = new NormDotProdSim()
}
