package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.matches.models.{SearchInfo, PepSpectraMatch}
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import ch.isbsib.proteomics.mzviz.theoretical.services.SequenceMongoDBService._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import play.api.libs.concurrent.Execution.Implicits._
import reactivemongo.core.commands.LastError
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._

import scala.concurrent.Future

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class SearchInfoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "searchInfo"
  val mainKeyName = "searchId"


  /**
   * insert a list of SearchInfo entries
   * @param entries to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(entries: SearchInfo): Future[LastError] = {
    collection.insert(entries)
  }

  /*
  /**
   * remove all entries from the mongodb
   * @param searchId the seach id
   * @return
   */
  def deleteAllBySearchId(searchId: SearchId): Future[Boolean] = {
    val query = Json.obj("searchId" -> searchId.value)
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
  }

  /**
   * retrieves all entries for a given source
   * @param searchId the search id
   * @return
   */
  def findAllSearchsBySearchId(searchId: SearchId): Future[Seq[SpectrumId]] = {
    val query = Json.obj("searchId" -> searchId.value)
    val projection = Json.obj("spectrumId" -> 1, "_id" -> 0)
    collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map(l => l.map(x => (x \ "spectrumId").as[SpectrumId]))
  }

  */

  object SearchInfoDBService extends Controller with MongoController {
    val default = new SearchInfoDBService(db)

    /**
     * get the default database
     * @return
     */
    def apply() = default


  }

}
