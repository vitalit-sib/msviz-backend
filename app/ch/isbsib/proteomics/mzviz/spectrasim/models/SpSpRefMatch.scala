package ch.isbsib.proteomics.mzviz.spectrasim.models

import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumRef}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class SpSpRefMatch(sp1: SpectrumRef, sp2: SpectrumRef, score: Double)
