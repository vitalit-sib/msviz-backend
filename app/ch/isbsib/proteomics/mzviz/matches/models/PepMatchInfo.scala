package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons.MassUnit
import ch.isbsib.proteomics.mzviz.modifications.ModifName

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2017, SIB Swiss Institute of Bioinformatics
 */


case class PepMatchInfo(
                         score: IdentScore,
                         numMissedCleavages: Option[Int],
                         moz: Option[Double],
                         massDiff: Option[Double],
                         massDiffUnit: Option[MassUnit],
                         rank: Option[Int],
                         totalNumIons: Option[Int],
                         chargeState: Option[Int],
                         isRejected: Option[Boolean],
                         modificationProbabilities: Option[Map[ModifName, String]],
                         highestModifProbability: Option[Map[ModifName, Double]]
                         )




