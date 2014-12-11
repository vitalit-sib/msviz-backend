package ch.isbsib.proteomics.mzviz.experimental.models

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
  * @param ref
 * @param peaks
 */

case class ExpMSnSpectrum (ref:RefSpectrum, peaks:Seq[ExpPeakMSn])
