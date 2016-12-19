package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.commons.{Moz, Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.experimental.{RunIdAndMozBin, RunId}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 * @param ref  runId
 * @param rt retention time
 * @param intensity
 * @param moz m/z
 */

case class Ms1EntryWithRef(ref:RunId, rt: RetentionTime, intensity: Intensity, moz:Moz)

// this is essentially the same class as Ms1Entry without the wrappers
// @TODO use Ms1Entry instead of Ms1Peak. Ms1Peak is used to work with the MySql database.
case class Ms1Peak(ref: String, rt: Double, moz: Double, int:Double)


case class Ms1Entry(rt: RetentionTime, intensity: Intensity, moz:Moz)

case class Ms1EntryList(ref: RunIdAndMozBin, ms1EntryList: Seq[Ms1Entry])