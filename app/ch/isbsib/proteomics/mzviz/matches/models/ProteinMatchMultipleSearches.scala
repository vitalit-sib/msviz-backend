package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

case class ProteinMatchMultipleSearches (dict:Map[AccessionCode,Seq[ProteinIdent]]){

  def add(proteinInfo:ProteinIdent): ProteinMatchMultipleSearches = {

    val newSeq= dict.getOrElse(proteinInfo.mainProt.proteinAC, Seq()) :+  proteinInfo
    val newDict=dict + (proteinInfo.mainProt.proteinAC -> newSeq)
    ProteinMatchMultipleSearches(newDict)
  }
}

