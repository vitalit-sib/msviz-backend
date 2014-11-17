package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.experimental.MSRun
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import reactivemongo.bson.BSONDocument
import scala.concurrent.Future
import JsonFormats._
/**
 * @author Alexandre Masselot
 */
object ExpMongoDBService  extends Controller with MongoController{
  def msnSpectraCollection: JSONCollection = db.collection[JSONCollection]("msnSpectra")

  def msRunSave(run:MSRun) = {
    val enumerator = Enumerator(run.msnSpectra: _*)

    msnSpectraCollection.bulkInsert(enumerator)
  }
}
