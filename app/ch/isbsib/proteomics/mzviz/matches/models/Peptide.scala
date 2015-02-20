package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.modifications.models.{ModificationRef}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class Peptide(
                    sequence: String,
                    molMass: Double,
                    modificationRefs: Vector[Seq[ModificationRef]]
                    )
