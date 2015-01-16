package ch.isbsib.proteomics.mzviz.matches.models

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

case class PepMatchInfo(
                         scoreMap: Map[String, Double],
                         numMissedCleavages: Option[Int],
                         massDiff: Option[Double],
                         // modifications: Modifications,
                         rank: Int,
                         totalNumIons: Option[Int],
                         isRejected: Option[Boolean]
                         // isDecoy: Boolean
                         )
