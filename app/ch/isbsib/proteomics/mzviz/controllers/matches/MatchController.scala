package ch.isbsib.proteomics.mzviz.controllers.matches

import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.modifications.ModifName

/**
 * Commons functionalities for our controllers
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
trait MatchController extends CommonController {
  /**
   * transform searchIds comma separated parameter into a set of sSearchId
   * @param searchIds
   * @return
   */
  def queryParamSearchIds(searchIds: String): Set[SearchId] = searchIds.split(",").toList.map(SearchId.apply).toSet

  /**
   * simply map an option of a string into an option of a ModifName
   * @param withModif
   * @return
   */
  def queryParamOModifName(withModif: Option[String]): Option[ModifName] =withModif.map(ModifName.apply)


}
