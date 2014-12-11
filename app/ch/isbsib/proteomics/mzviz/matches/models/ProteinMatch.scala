package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.ProteinAC

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
case class ProteinMatch (AC: ProteinAC, previousAA: String, nextAA: String, startPos: Int, endPos: Int)
