package ch.isbsib.proteomics.mzviz.matches.models.maxquant

/**
 * @author Roman Mylonas & Trinidad Martín
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class EvidenceTableEntry ( id:String,
                                sequence:String,
                                experiment:String,
                                molMass:Option[Double] ,
                                score:Double,
                                missedCleavages:Option[Int],
                                massDiff: Double,
                                chargeState: Option[Int])
