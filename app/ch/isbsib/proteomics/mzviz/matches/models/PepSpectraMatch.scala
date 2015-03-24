package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

case class PepSpectraMatch(searchId: SearchId,
                           spectrumId: SpectrumId,
                           pep: Peptide,
                           matchInfo: PepMatchInfo,
                           proteinList: Seq[ProteinMatch]) {
  /**
   * returns a PSM, but with the proteinList containing only the given AC
   * @param ac
   * @return
   */
  def extractAC(ac: AccessionCode): PepSpectraMatch = {
    PepSpectraMatch(
      searchId,
      spectrumId,
      pep,
      matchInfo,
      proteinList.filter(_.proteinRef.AC == ac).take(1)
    )
  }
}

