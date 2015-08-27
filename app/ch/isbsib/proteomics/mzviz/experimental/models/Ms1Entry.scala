package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.commons.{Moz, Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.experimental.RunId

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 * @param ref  runId
 * @param rt retention time
 * @param intensity
 * @param moz m/z
 */

case class Ms1Entry(ref:RunId, rt: RetentionTime,intensity: Intensity, moz:Moz )
