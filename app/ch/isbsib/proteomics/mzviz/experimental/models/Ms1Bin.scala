package ch.isbsib.proteomics.mzviz.experimental.models

import ch.isbsib.proteomics.mzviz.commons.{Moz, Intensity, RetentionTime}

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class Ms1Bin(ref: String, mozList: Seq[Moz], intList: Seq[Intensity], rtList: Seq[RetentionTime])
