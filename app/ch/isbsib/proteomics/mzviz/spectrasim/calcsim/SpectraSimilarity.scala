package ch.isbsib.proteomics.mzviz.spectrasim.calcsim

import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpMatch

/**
 * Trait for calculate similarity between 2 spectra, setting a peak tolerance
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
trait SpectraSimilarity {

  /**
   * calculate spectra similartity between 2 spectra
   *
   * @param sp1 spectra 1
   * @param sp2 spectra 2
   * @param absTolerance peak match tolerance in Dalton (default 0.5)
   * @return similiraty between 0 and 1
   */
  def calcSimilarity(sp1: ExpMSnSpectrum, sp2: ExpMSnSpectrum, absTolerance: Double): SpSpMatch

  /**
   * calculates spectra similarity between one spectra and a list of spectra
   *
   * @param sp1
   * @param sp2List
   * @param absTolerance
   * @return
   */
  def calcSimilarityList(sp1: ExpMSnSpectrum, sp2List: Seq[ExpMSnSpectrum], absTolerance: Double): Seq[SpSpMatch]

}
