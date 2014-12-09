package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.commons._


/**
 * @author Roman Mylonas
 */
case  class RefSpectrum(scanNumber:ScanNumber, precursor:ExpPeakPrecursor, title:String, idRun:Option[IdRun])
