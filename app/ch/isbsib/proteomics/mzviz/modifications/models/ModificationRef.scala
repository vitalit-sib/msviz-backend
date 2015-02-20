package ch.isbsib.proteomics.mzviz.modifications.models

import ch.isbsib.proteomics.mzviz.modifications.{ModifSource, ModifAC}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class ModificationRef(AC: ModifAC, modifSource: ModifSource)
