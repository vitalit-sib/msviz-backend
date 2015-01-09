package ch.isbsib.proteomics.mzviz.matches.models

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
case class ProteinMatch (proteinRef: ProteinRef, previousAA: String, nextAA: String, startPos: Int, endPos: Int)
