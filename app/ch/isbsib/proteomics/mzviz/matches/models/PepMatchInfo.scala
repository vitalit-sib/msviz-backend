package ch.isbsib.proteomics.mzviz.matches.models

import cern.jet.stat.Probability
import ch.isbsib.proteomics.mzviz.commons.MassUnit
import ch.isbsib.proteomics.mzviz.modifications.ModifName

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2017, SIB Swiss Institute of Bioinformatics
 */


sealed trait ModifStatus {def value: String}
case object MAIN extends ModifStatus {val value = "MAIN"}
case object CONFLICT extends ModifStatus {val value = "CONFLICT"}

case class ModifInfo(position: Int, modifProb: Double, status: ModifStatus)

case class PepMatchInfo(
                         score: IdentScore,
                         numMissedCleavages: Option[Int],
                         correctedMoz: Option[Double],
                         correctedMolMass: Option[Double],
                         massDiff: Option[Double],
                         massDiffUnit: Option[MassUnit],
                         rank: Option[Int],
                         totalNumIons: Option[Int],
                         chargeState: Option[Int],
                         isRejected: Option[Boolean],
                         isContaminant: Option[Boolean],
                         modificationProbabilities: Option[Map[ModifName, String]],
                         highestModifProbability: Option[Map[ModifName, Double]],
                         // @TODO this is a kind of redundant with Peptide.modificationNames. The one here should be the main one.
                         modificationInfos: Option[Map[ModifName, Seq[ModifInfo]]]
                         )




