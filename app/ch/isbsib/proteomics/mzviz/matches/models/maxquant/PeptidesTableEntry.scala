package ch.isbsib.proteomics.mzviz.matches.models.maxquant

/**
 * @author Roman Mylonas & Trinidad Martín
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class PeptidesTableEntry (evidenceId:Int,
                               previousAA:Option[String],
                               nextAA:Option[String],
                               startPos:Int,
                               endPos:Int,
                               isDecoy:Option[Boolean])
