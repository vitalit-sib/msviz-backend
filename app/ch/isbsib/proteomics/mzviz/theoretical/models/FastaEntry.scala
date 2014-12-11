package ch.isbsib.proteomics.mzviz.theoretical.models

import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}

/**
 *
 * a fasta entry with an accesison code, a sequence and a source.
 * The source will be somwthing like "uniprot_sprot_20140101"
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
case class FastaEntry ( ac: AccessionCode,  sequence: String,  source: Option[SequenceSource]=None) {
  override def toString = s">${ac.value}\n$sequence"

}
