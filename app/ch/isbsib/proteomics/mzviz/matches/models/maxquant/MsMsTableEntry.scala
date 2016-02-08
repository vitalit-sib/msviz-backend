package ch.isbsib.proteomics.mzviz.matches.models.maxquant

import ch.isbsib.proteomics.mzviz.experimental.RunId

/**
 * @author Roman Mylonas & Trinidad Martín
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class MsMsTableEntry (runId: RunId,id: Int, score: Double)
