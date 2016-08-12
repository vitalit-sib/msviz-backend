package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.matches.SearchId
import reactivemongo.api.DefaultDB
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class CommonMatchService(val db: DefaultDB) {
  val matchService = new MatchMongoDBService(db)
  val proteinMatchService = new ProteinMatchMongoDBService(db)
  val searchInfoService = new SearchInfoDBService(db)

  def deleteAllMatchInfo(searchId: SearchId): Future[Boolean] ={
    for {
      delMatch <- matchService.deleteAllBySearchId(searchId)
      delProts <- proteinMatchService.deleteAllBySearchId(searchId)
      delSearchId <- searchInfoService.delete(searchId)
    } yield {
      delMatch & delProts & delSearchId
    }
  }
}
