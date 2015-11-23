package ch.isbsib.proteomics.mzviz.matches.models.maxquant

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * @author Roman Mylonas & Trinidad Martín
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class ProteinGroupsTable (bestMsMs:List[String], uniquePeptides: Int, majorityProtein: List[String], msCount:Int, runId:RunId)
