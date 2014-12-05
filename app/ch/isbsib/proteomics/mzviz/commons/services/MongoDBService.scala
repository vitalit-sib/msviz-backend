package ch.isbsib.proteomics.mzviz.commons.services

import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, RefSpectrum}
import ch.isbsib.proteomics.mzviz.experimental.{ScanNumber, IdRun, MSRun}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api._
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.bson._
import reactivemongo.core.commands.{LastError, GetLastError, RawCommand, Count}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author Alexandre Masselot
 */
trait MongoDBService {
  val db: DefaultDB
  val collectionName: String

  def collection: JSONCollection = db.collection[JSONCollection](collectionName)


  def indexes: List[Index]

  /**
   * Ensure we have the correct indexes
   */
  def setupIndexes:Unit = {
    Logger.info(s"building mongo indexes for $collectionName")

    for {
      idx <- indexes
    } {
      collection.indexesManager.ensure(idx).map {
        b =>
          if (b)
            Logger.info(s"index [${idx.name}] was created")
          else
            Logger.info(s"index [${idx.name}] already exists")
      }

    }
  }


  setupIndexes
}

case class MongoNotFoundException(message:String) extends Exception(message)