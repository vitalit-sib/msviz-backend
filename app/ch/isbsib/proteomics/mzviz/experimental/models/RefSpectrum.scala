package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.commons._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
case  class RefSpectrum(scanNumber:ScanNumber, precursor:ExpPeakPrecursor, title:String, runId:Option[RunId])
