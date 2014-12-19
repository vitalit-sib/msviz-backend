package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons.{SpectraId, SpectraSource}
import ch.isbsib.proteomics.mzviz.matches.SearchId

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */

case class PepSpectraMatch (searchId: SearchId, spId: SpectraId, spSource: SpectraSource, pep: Peptide, matchInfo: PepMatchInfo, proteinList: Seq[ProteinMatch])
