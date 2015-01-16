package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

case class PepSpectraMatch (searchId: SearchId,
                            spectrumId: SpectrumId,
                            pep: Peptide,
                            matchInfo: PepMatchInfo,
                            proteinList: Seq[ProteinMatch])
