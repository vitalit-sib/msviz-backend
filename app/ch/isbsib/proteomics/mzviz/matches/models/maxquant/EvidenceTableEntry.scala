package ch.isbsib.proteomics.mzviz.matches.models.maxquant

/**
 * @author Roman Mylonas & Trinidad Martín
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class EvidenceTableEntry ( id:Int,
                                sequence:String,
                                experiment:String,
                                molMass:Option[Double] ,
                                score:Double,
                                missedCleavages:Option[Int],
                                massDiff: Option[Double],
                                chargeState: Option[Int],
                                ac:String,
                                pepId:Int)
