package ch.isbsib.proteomics.mzviz.matches.services

import ch.isbsib.proteomics.mzviz.commons.helpers.CommonFunctions
import ch.isbsib.proteomics.mzviz.commons.{Charge, MolecularMass, Moz}
import ch.isbsib.proteomics.mzviz.commons.services.{MongoDBService, MongoNotFoundException}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumId, SpectrumRef}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.modifications.ModifName
import ch.isbsib.proteomics.mzviz.modifications.services.JsonModificationFormats._
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.{PepSpectraMatch, PepSpectraMatchWithSpectrumRef, ProteinRef}
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import ch.isbsib.proteomics.mzviz.theoretical.{AccessionCode, ProteinIdentifier, SequenceSource}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.{BSONArray, BSONDocument, BSONString}
import reactivemongo.core.commands._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2017, SIB Swiss Institute of Bioinformatics
 */
class MatchMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "psm"
  val mainKeyName = "searchId"

  setIndexes(List(
    new Index(
      Seq("searchId" -> IndexType.Ascending),
      name = Some("searchId_source"),
      unique = false),
    new Index(
      Seq("proteinList.proteinRef.AC" -> IndexType.Ascending, "proteinList.proteinRef.source" -> IndexType.Ascending),
      name = Some("proteinRef"),
      unique = false),
    new Index(
      Seq("pep.modificationNames" -> IndexType.Ascending),
      name = Some("modificationNames"),
      unique = false)
  ))

  /**
   * insert a list of Match entries
   * @param matches to be inserted
   * @return a Future of the number of entries loaded
   */
  def insert(matches: Seq[PepSpectraMatch]): Future[Int] = {
    val searchIds = matches.map(_.searchId.value).toSet

    for {
      c <- checkIfAnyKeyExist(searchIds)
      n <- collection.bulkInsert(Enumerator(matches: _*))
    } yield n
  }

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
   * remove all entries from the mongodb
   * @param searchIds mutliple search ids
   * @return
   */
  def deleteAllBySearchIds(searchIds: Set[SearchId]): Future[Boolean] = {
    val query = Json.obj("searchId" -> Json.obj("$in" -> searchIds.toList))
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
  def findAllSpectrumIdBySearchId(searchId: SearchId): Future[Seq[SpectrumId]] = {
    val query = Json.obj("searchId" -> searchId.value)
    val projection = Json.obj("spectrumId" -> 1, "_id" -> 0)
    collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map(l => l.map(x => (x \ "spectrumId").as[SpectrumId]))
  }

  /**
   * retrieves all entries for a given source
   * @param searchId the search id
   * @return
   */
  def findAllBySearchIdAndSpectrumId(searchId: SearchId, spectrumUniqueId: SpectrumUniqueId): Future[Seq[PepSpectraMatch]] = {

    val query = Json.obj("searchId" -> searchId.value, "spectrumId.id" -> spectrumUniqueId.value)
    collection.find(query)
      .cursor[PepSpectraMatch]
      .collect[List]()
  }

  /**
   * retrieves all entries for a given source
   * @param searchId the search id
   * @return
   */
  def findAllPSMBySearchId(searchId: SearchId): Future[Seq[PepSpectraMatch]] = {
    val query = Json.obj("searchId" -> searchId.value)

    collection.find(query).cursor[PepSpectraMatch].collect[List]()
  }


  /**
   *
  db.psm.aggregate([
      {$match:{'searchId':'test-a', 'proteinList.proteinRef.AC':'GNAO_HUMAN', 'proteinList.proteinRef.source':'TODO'}},
      {$unwind:'$proteinList'},
      {$match:{'proteinList.proteinRef.AC':'GNAO_HUMAN', 'proteinList.proteinRef.source':'TODO'}},
      {$project:{proteinPosition:'$proteinList', runId:1, spId:1, pep:1, matchInfo:1, _id:0}}
    ]).pretty()
   * @param searchIds search to filter on
   * @param source data source
   * @param accessionCode entry AC
   * @return
   */
  def findAllPSMsByProtein(accessionCode: AccessionCode,
                           source: Option[SequenceSource] = None,
                           searchIds: Option[Set[SearchId]] = None,
                           notRejected: Option[Boolean] = None): Future[Seq[PepSpectraMatch]] = {

    Logger.info(s"accessionCode=$accessionCode, searchIds = $searchIds")
    val query = Json.obj("proteinList.proteinRef.AC" -> accessionCode.value) ++
      (source match {
        case Some(SequenceSource(src)) => Json.obj("proteinList.proteinRef.source" -> src)
        case _ => Json.obj()
      }) ++
      (searchIds match {
        case Some(ssids) => Json.obj("searchId" -> Json.obj("$in" -> ssids.toList.map(_.value)))
        case _ => Json.obj()
      }) ++
      (notRejected match {
        case Some(notRej) => Json.obj("matchInfo.isRejected" -> false)
        case _ => Json.obj()
      })

    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    val writer = mapper.writerWithDefaultPrettyPrinter
    val json = writer.writeValueAsString(query)

    collection.find(query).cursor[PepSpectraMatch].collect[List]()
  }

  /**
   * decorated all PePMAtchSpectra with Spectrum refm turning them into PepSpectraMatchWithSpectrumRef
   * @param psms  list of psms
   * @return
   */
  def enrichWithSpectrumRefs(psms: Seq[PepSpectraMatch]): Future[Seq[PepSpectraMatchWithSpectrumRef]] = {

    implicit def searchIdToRunId(searchId: SearchId): RunId = RunId(searchId.value)

    //println (s"psms=$psms")
    val runIds:Set[RunId] = psms.map(p => RunId(p.searchId.value)).toSet
    //println(s"runIds = $runIds")

    val futSpectrumRefs: Future[Seq[SpectrumRef]] = new ExpMongoDBService(db).findAllSpectraRefByrunId(runIds)

    val futRunId2speRefDict: Future[Map[RunId, Map[SpectrumId, SpectrumRef]]] =
      futSpectrumRefs.map({ spectrumRefs =>
        spectrumRefs
          .groupBy(_.spectrumId.runId) // map[RunIds, Seq[SpectrumRefs]
          .map({
          case (runId, spRefs) => //now, transform the list of spref to a map spectrumId -> spRef
            (runId, spRefs.map(spRef => (spRef.spectrumId, spRef)).toMap)
        })
      })

    for {
      dict <- futRunId2speRefDict
    } yield {
      psms.map({ psm =>
        PepSpectraMatchWithSpectrumRef(psm, dict(psm.searchId)(psm.spectrumId))
      })
    }
  }

  def qFilter(searchIds: Set[SearchId]) = searchIds.toList match {
    case x :: Nil => BSONDocument("searchId" -> x.value)
    case xs => BSONDocument("searchId" -> BSONDocument("$in" -> BSONArray(xs.map(id => BSONString(id.value)))))
  }
  def qFilter(withModification: Option[ModifName]) = withModification match {
    case Some(name) => BSONDocument("pep.modificationNames" -> BSONArray(BSONString(name.value)), "matchInfo.rank" -> 1)
    case None => BSONDocument()
  }

  /**
   * retrieves a list of Proteins from one source
   * NB: the identifiers field is not populated by the match database (information is not available)
   * @param searchIds searches identifier
   * @return list of Proteins
   */

  def listProteinRefsBySearchIds(searchIds: Set[SearchId], withModification: Option[ModifName] = None): Future[Seq[ProteinRef]] = {
    val qMatch = qFilter(searchIds)++ qFilter(withModification)

    val command = RawCommand(BSONDocument(
      "aggregate" -> collectionName,
      "pipeline" -> BSONArray(
        BSONDocument("$match" -> qMatch),
        BSONDocument("$project" -> BSONDocument("proteinList.proteinRef.AC" -> 1, "proteinList.proteinRef.source" -> 1, "searchId" -> 1, "_id" -> 0)),
        BSONDocument("$unwind" -> "$proteinList"),
        BSONDocument("$project" -> BSONDocument("AC" -> "$proteinList.proteinRef.AC", "source" -> "$proteinList.proteinRef.source"))
      )
    ))

    db.command(command).map({
      doc =>
        doc.getAs[List[BSONDocument]]("result").get.map({
          elDoc: BSONDocument =>
            ProteinRef(AccessionCode(elDoc.getAs[String]("AC").get),
              Set(),
              Some(SequenceSource(elDoc.getAs[String]("source").get)))
        }).distinct
    })
  }

  /**
   * retrieves a list of Proteins from one source
   * NB: the identifiers field is not populated by the match database (information is not available)
   *
  db.psm.aggregate([{$match:{ 'searchId':'mascot:F001303'}}, {$project:{'modif':'$pep.modificationNames', _id:0}}, {$unwind:'$modif'}, {$unwind:'$modif'}, {$match:{'modif':{$ne:[]}}}, {$group: {_id:'$modif', count:{$sum:1}}}]).pretty()   * @param searchIds a set of search
   * @return list of Modification
   */
  def findAllModificationsBySearchIds(searchIds: Set[SearchId]): Future[Map[ModifName, Int]] = {
    val command = RawCommand(BSONDocument(
      "aggregate" -> collectionName,
      "pipeline" -> BSONArray(
        BSONDocument("$match" -> qFilter(searchIds)),
        BSONDocument("$project" -> BSONDocument(
          "modif" -> "$pep.modificationNames",
          "_id" -> 0)),
        BSONDocument("$unwind" -> "$modif"),
        BSONDocument("$unwind" -> "$modif"),
        BSONDocument("$match" -> BSONDocument("modif" -> BSONDocument("$ne" -> BSONArray.empty))),
        BSONDocument("$group" -> BSONDocument("_id" -> "$modif", "count" -> BSONDocument("$sum" -> 1)))
      )
    ))

    db.command(command).map({
      doc =>
        doc.getAs[List[BSONDocument]]("result").get.map({
          elDoc: BSONDocument =>
            (ModifName(elDoc.getAs[String]("_id").get), elDoc.getAs[Int]("count").get)
        }).toMap
    })


  }

  /**
   * Get the list of data sources
   * @return
   */
  def listRunIds: Future[Seq[RunId]] = {
    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "spectrumId.runId"))
    db.command(command)
      .map({
      doc =>
        doc.getAs[List[String]]("values").get
          .map {
          i => RunId(i)
        }
    })
  }

  /**
   * count the number of Entries
   * @return
   */
  def countEntries: Future[Int] = {
    db.command(Count(collectionName))
  }

  /**
   * count the number of data sources
   * @return
   */
  def countRunIds: Future[Int] = {
    listRunIds.map(_.size)
  }

  /**
   * a maps with various counts
   * @return
   */
  def stats: Future[Map[String, Int]] = {
    for {
      nSources <- countRunIds
      nEntries <- countEntries
    } yield {
      Map("sources" -> nSources, "entries" -> nEntries)
    }
  }

  /**
    * Find all spectra and molecular masses corresponding to a certain run
    *
    * @param runId
    * @return
    */
  def findSpectrumIdWithMolMass(runId:RunId): Future[Seq[(SpectrumUniqueId, MolecularMass)]] = {

    val query = Json.obj(
      "spectrumId.runId" -> runId.value,
      "matchInfo.correctedMolMass" -> Json.obj("$exists" -> true)
    )

    val projection = Json.obj(
      "spectrumId.id" -> 1,
      "matchInfo.correctedMolMass" -> 1
    )

    val futureSpList:Future[Seq[(String, Option[Double])]] = collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map(lo => lo.map({ o =>
        (
          Json.fromJson[String](o \ "spectrumId" \ "id").asOpt.get,
          Json.fromJson[Double](o \ "matchInfo" \ "correctedMolMass").asOpt
          )
      }))

    futureSpList.map(l => l.filter(sp => sp._2.isDefined).map({
      case (sp:String, mm:Option[Double]) =>
        (SpectrumUniqueId(sp), MolecularMass(mm.get))
    }))
  }

}

object MatchMongoDBService extends Controller with MongoController {
  val default = new MatchMongoDBService(db)

  /**
   * get the default db/collection
   * @return
   */
  def apply() = default

}
