package ch.isbsib.proteomics.mzviz.controllers

import ch.isbsib.proteomics.mzviz.matches.models.PepSpectraMatch

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object TsvFormats {
  /**
   * putput a string with tsv separated psm
   * @param psms
   * @return
   */
  def toTsv(psms:Seq[PepSpectraMatch]):String = {

    val sb = new StringBuilder
    sb.append("searchId\tpeptideSequence\tAC\tmassDiff\trunId\tspectrumId\n")

    psms.foreach({ psm =>
      sb.append(List(psm.searchId.value, psm.pep.sequence, psm.proteinList.map(_.proteinRef.AC.value).mkString(","), psm.matchInfo.massDiff.getOrElse("NA"), psm.spectrumId.runId.value, psm.spectrumId.id.value).mkString("\t")+"\n")
    })
    sb.toString()
  }

}
