package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons.SpectraId

/**
 * Created by Roman Mylonas on 21/11/14.
 */

case class PepSpectraMatch (spId: SpectraId, pep: Peptide, matchInfo: PepMatchInfo, proteinList: Seq[ProteinMatch])
