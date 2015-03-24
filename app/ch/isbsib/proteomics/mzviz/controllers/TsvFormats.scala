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
   * @param showFirstProtMatchInfo will display the first proteinmatch info (AC, position, flanking residue ...). to be combined with psm.extractAC
   * @return
   */
  def toTsv(psms: Seq[PepSpectraMatch], showFirstProtMatchInfo: Boolean = false): String = {

    val sb = new StringBuilder

    val headerFields: List[String] = List[String]("searchId", "peptideSequence", "massDiff", "runId", "spectrumId") ++
      (if (showFirstProtMatchInfo) {
        List("prot.AC", "prot.startPos", "prot.endPos", "prot.previousAA", "prot.nextAA")
      } else {
        Nil
      })

    def extractVal: ((PepSpectraMatch) => List[Any]) = { psm: PepSpectraMatch =>
      List(psm.searchId.value, psm.pep.sequence, psm.matchInfo.massDiff.getOrElse("NA"), psm.spectrumId.runId.value, psm.spectrumId.id.value) ++
        (if (showFirstProtMatchInfo) {
          val pm = psm.proteinList.head
          List(pm.proteinRef.AC, pm.startPos, pm.endPos, pm.previousAA.getOrElse("."), pm.nextAA.getOrElse("."))
        } else {
          Nil
        })
    }

    sb.append(headerFields.mkString("\t") + "\n")
    psms.foreach({ psm =>
      sb.append(extractVal(psm).mkString("\t") + "\n")
    })
    sb.toString()
  }

}
