package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons.{SpectraId, SpectraSource}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */

case class PepSpectraMatch (spId: SpectraId, spSource: SpectraSource, pep: Peptide, matchInfo: PepMatchInfo, proteinList: Seq[ProteinMatch])
