package ch.isbsib.proteomics.mzviz.theoretical.models

import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * Created by tmartinc on 21/11/14.
 * @author Trinidad Martín
 */
case class FastaEntry ( ac: AccessionCode,  sequence: String) {
  override def toString = s">${ac.value}\n$sequence"

}
