package ch.isbsib.proteomics.mzviz.matches.models.maxquant

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * @author Roman Mylonas & Trinidad Martín
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class ProteinGroupsTableEntry (bestMsMs:List[Int], uniquePeptides: Map[RunId,Int], majorityProtein: List[String], msCount:Map[RunId,Int])
