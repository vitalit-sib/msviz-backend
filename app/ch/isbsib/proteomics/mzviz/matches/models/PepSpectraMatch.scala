package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons.{SpectraId, SpectraSource}

/**
 * Created by Roman Mylonas
 */

case class PepSpectraMatch (spId: SpectraId, spSource: SpectraSource, pep: Peptide, matchInfo: PepMatchInfo, proteinList: Seq[ProteinMatch])
