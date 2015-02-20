package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.modifications.models.Modification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class Peptide(
                    sequence: String,
                    molMass: Double,
                    modifications: Seq[Seq[Modification]]
                    )
