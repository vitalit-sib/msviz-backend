package ch.isbsib.proteomics.mzviz.matches.models

/**
 * Created by Roman Mylonas on 21/11/14.
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
