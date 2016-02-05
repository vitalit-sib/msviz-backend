package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.commons._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class SpectrumRef(scanNumber:Option[ScanNumber], precursor:ExpPeakPrecursor, title:String, spectrumId:SpectrumId)
