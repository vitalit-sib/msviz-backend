package ch.isbsib.proteomics.mzviz.theoretical.models

import ch.isbsib.proteomics.mzviz.theoretical.SequenceSource

/**
 * aggregated statistics, per sequence source (loaded fasta), such as number of entries, residues...
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, Swiss Institute of Bioinformatics
 */
case class SequenceSourceStats(source:SequenceSource, nbEntries:Int, nbResidues:Int)
