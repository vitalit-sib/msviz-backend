package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

case class ProteinMatchMultipleSearches (dict:Map[AccessionCode,Map[SearchId,Seq[ProteinIdent]]]){

  def add(searchId:SearchId,proteinInfo:ProteinIdent): ProteinMatchMultipleSearches = {

    val newMapSearchProt= dict.getOrElse(proteinInfo.mainProt.proteinAC, Map())
    val newSeq= newMapSearchProt.getOrElse(searchId, Seq()) :+  proteinInfo
    val newMap= newMapSearchProt + (searchId -> newSeq)
    val newDict=dict + (proteinInfo.mainProt.proteinAC -> newMap)
    ProteinMatchMultipleSearches(newDict)
  }
}

