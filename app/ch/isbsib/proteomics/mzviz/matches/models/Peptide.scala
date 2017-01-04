package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.modifications.ModifName

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2017, SIB Swiss Institute of Bioinformatics
 */
case class Peptide(
                    sequence: String,
                    molMass: Option[Double],
                    modificationNames: Vector[Seq[ModifName]]
                    )
