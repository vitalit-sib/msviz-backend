package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.commons.RetentionTime

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
  * @param ref point to spectrum id and run
 * @param peaks peak list
 */

case class ExpMSnSpectrum (ref:SpectrumRef, peaks:List[ExpPeakMSn])

case class ExpMs1Spectrum(spId:SpectrumId, retentionTime: RetentionTime, peaks:List[ExpPeakMS1])
