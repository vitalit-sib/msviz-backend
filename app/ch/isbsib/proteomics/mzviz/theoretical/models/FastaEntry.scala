package ch.isbsib.proteomics.mzviz.theoretical.models

/**
 * Created by tmartinc on 21/11/14.
 * @author Trinidad Martín
 */
case class FastaEntry ( ac: String,  sequence: String) {
  override def toString = s">$ac\n$sequence"

}
