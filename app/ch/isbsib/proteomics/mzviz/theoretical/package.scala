package ch.isbsib.proteomics.mzviz

/**
 * @author Alexandre Masselot
 */
package object theoretical {
  //protein accession code
  case class AccessionCode(value:String) extends AnyVal
  //the fasta source + version
  case class SequenceSource(value:String) extends AnyVal
}
