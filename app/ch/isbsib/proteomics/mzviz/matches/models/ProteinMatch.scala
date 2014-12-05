package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.ProteinAC

/**
 * Created by Roman Mylonas
 */
case class ProteinMatch (AC: ProteinAC, previousAA: String, nextAA: String, startPos: Int, endPos: Int)
