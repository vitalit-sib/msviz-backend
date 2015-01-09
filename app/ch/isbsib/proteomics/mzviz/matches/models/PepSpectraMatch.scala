package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons.SpectraId
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */

case class PepSpectraMatch (searchId: SearchId, runId: RunId, spId: SpectraId, pep: Peptide, matchInfo: PepMatchInfo, proteinList: Seq[ProteinMatch])
