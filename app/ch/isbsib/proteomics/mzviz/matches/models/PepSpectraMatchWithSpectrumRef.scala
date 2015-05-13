package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumRef, SpectrumId}
import ch.isbsib.proteomics.mzviz.matches.SearchId

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class PepSpectraMatchWithSpectrumRef(searchId:SearchId,
                                     spectrumId:SpectrumId,
                                     pep:Peptide, matchInfo:PepMatchInfo,
                                     proteinList: Seq[ProteinMatch],
                                     spectrumRef:SpectrumRef) extends PepSpectraMatch(searchId,spectrumId, pep, matchInfo, proteinList){


}
