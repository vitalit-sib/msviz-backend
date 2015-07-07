package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

case class ProteinMatchMultipleSearches (dict:Map[AccessionCode,Map[SearchId,ProteinIdent]]){

  def add(searchId:SearchId,proteinInfo:ProteinIdent): ProteinMatchMultipleSearches = {

    val newMapSearchProt= dict.getOrElse(proteinInfo.mainProt.proteinAC, Map())
    val newProt= newMapSearchProt.getOrElse(searchId,proteinInfo)
    println(newProt)
    val newMap= newMapSearchProt + (searchId -> newProt)
    val newDict=dict + (proteinInfo.mainProt.proteinAC -> newMap)
    ProteinMatchMultipleSearches(newDict)
  }
}

