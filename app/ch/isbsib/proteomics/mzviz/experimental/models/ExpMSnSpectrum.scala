package ch.isbsib.proteomics.mzviz.experimental.models

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
  * @param ref point to spectrum id and run
 * @param peaks peak list
 */

case class ExpMSnSpectrum (ref:SpectrumRef, peaks:List[ExpPeakMSn])
