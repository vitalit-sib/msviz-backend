package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons.MassUnit

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


case class PepMatchInfo(
                         score: IdentScore,
                         numMissedCleavages: Option[Int],
                         massDiff: Option[Double],
                         massDiffUnit: Option[MassUnit],
                         rank: Option[Int],
                         totalNumIons: Option[Int],
                         chargeState: Option[Int],
                         isRejected: Option[Boolean]
                         )




