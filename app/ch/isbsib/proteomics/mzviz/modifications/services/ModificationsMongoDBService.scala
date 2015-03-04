package ch.isbsib.proteomics.mzviz.modifications.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.modifications.importer.UnimodParser
import ch.isbsib.proteomics.mzviz.modifications.models.Modification
import ch.isbsib.proteomics.mzviz.theoretical.models.{SequenceSourceStats, FastaEntry}
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, SequenceSource}
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api._
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.{BSONArray, BSONString, BSONDocument}
import reactivemongo.core.commands.{LastError, Remove, RawCommand, Count}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ModificationsMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "modifications"
  val mainKeyName="modificationName"

  //Obtain unimod dictionary
  val unimodFile = ("/Users/tmartinc/Documents/msViz/2/msviz-backend/test/resources/unimod.xml")
  val unimodDictionary=UnimodParser(unimodFile).getDictionary
  val keys=unimodDictionary.keys

  val modificationMap=createModificationObject.toMap

  /**
   * return map of ModificationName->Modification
   * @return
   */
  def createModificationObject ={
    val l=for {key<-keys}
    yield
    {
      (ModifName(key), Modification(unimodDictionary.get(key).get))
    }
    l
  }

}

object ModificationsMongoDBService extends Controller with MongoController {
  val default = new ModificationsMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default


}