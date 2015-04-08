package ch.isbsib.proteomics.mzviz.spectrasim.models

import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class SpSpMatch(sp1: ExpMSnSpectrum, sp2: ExpMSnSpectrum, similarity: Double)
