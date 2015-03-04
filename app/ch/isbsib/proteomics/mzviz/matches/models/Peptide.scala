package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.modifications.ModifName

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class Peptide(
                    sequence: String,
                    molMass: Double,
                    modificationNames: Vector[Seq[ModifName]]
                    )
