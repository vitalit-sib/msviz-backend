package ch.isbsib.proteomics.mzviz

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
package object theoretical {

  //protein accession code
  case class AccessionCode(value: String) extends AnyVal {
    override def toString = value
  }

  //the fasta source + version
  case class SequenceSource(value: String) extends AnyVal {
    override def toString = value
  }

  // fasta number of sequences from source
  case class NumDatabaseSequences(value: Int) extends AnyVal {
    override def toString = value.toString
  }

}
