package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.commons.helpers.CommonFunctions
import ch.isbsib.proteomics.mzviz.commons.services.{MongoDBService, MongoNotFoundException}
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, ExpPeakMSn, SpectrumId, SpectrumRef}
import ch.isbsib.proteomics.mzviz.experimental.{MSRun, RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import play.api.Play
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.BSONFormats._
import reactivemongo.api._
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson._
import reactivemongo.core.commands.{Count, LastError, RawCommand}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMongoDBService(val db: DefaultDB) extends MongoDBService {
  val collectionName = "msnSpectra"
  val mainKeyName = "ref.spectrumId.runId"

  setIndexes(List(
    new Index(
      Seq("ref.spectrumId.runId" -> IndexType.Ascending), // "ref.title" -> IndexType.Ascending),
      name = Some("runId")),
    new Index(
      Seq("ref.spectrumId.runId" -> IndexType.Ascending, "ref.spectrumId.id" -> IndexType.Ascending), // "ref.title" -> IndexType.Ascending),
      name = Some("ref.spectrumId"),
      unique = true)
  ))

  // "ref.title" -> IndexType.Ascending),

  //we put a implicit JSON serilizer here, the JSON Mongo format is difference from the JSON web format
  // peak array are serialized for sake of speed
  implicit val formatExpMSnSpectrum = new Format[ExpMSnSpectrum] {

    override def reads(json: JsValue): JsResult[ExpMSnSpectrum] = {
      val ref = (JsPath \ "ref").read[SpectrumRef].reads(json).get
      val msLevel = MSLevel(json.validate[Int]((JsPath \ "peaks" \ "msLevel").read[Int]).get)

      //re-assemble the peaks
      val mozs: List[Double] = decode64(json.validate[String]((JsPath \ "peaks" \ "mozs").read[String]).get)
      val intensities: List[Double] = decode64(json.validate[String]((JsPath \ "peaks" \ "intensities").read[String]).get)
      val intensityRanks: List[Int] = decode64(json.validate[String]((JsPath \ "peaks" \ "intensityRanks").read[String]).get)

      val peaks: List[ExpPeakMSn] =
        for {
          ((m: Double, i: Double), r: Int) <- mozs.zip(intensities).zip(intensityRanks)
        } yield {
          ExpPeakMSn(moz = Moz(m), intensity = Intensity(i), intensityRank = IntensityRank(r), msLevel = msLevel)
        }

      JsSuccess(
        ExpMSnSpectrum(ref = ref, peaks = peaks)
      )

    }

    def writes(o: ExpMSnSpectrum) = {
      val msLevel:Int = o.peaks.headOption.map(_.msLevel.value).getOrElse(-1)
      Json.obj(
        "ref" -> o.ref,
        "peaks" -> Json.obj(
          "msLevel" -> msLevel,
          "mozs" -> encode64[Double](o.peaks.map(_.moz.value)),
          "intensities" -> encode64[Double](o.peaks.map(_.intensity.value)),
          "intensityRanks" -> encode64[Int](o.peaks.map(_.intensityRank.value))
        )
      )
    }
  }


  /**
   * Insert all Msn spectra from an iterator using a cetrain buffer size
   *
   * @param ms2Iterator
   * @param runId
   * @return
   */
  def insertMs2spectra(ms2Iterator: Iterator[ExpMSnSpectrum], runId: RunId): Future[Int] = {

    // number of spectra which are parsed before inserting
    val bufferSize =  if(Play.maybeApplication.isDefined){
      Play.current.configuration.getString("experimental.ms2.buffer").get.toInt
    } else 50

    // split the iterator into slices
    val slidingIt = ms2Iterator.sliding(bufferSize, bufferSize)

    var resList:ListBuffer[Future[Int]] = ListBuffer()

    // loop through all slices
    while(slidingIt.hasNext){
      val someList = slidingIt.next()

      resList += this.insert(new MSRun(runId, someList.toSeq))
    }

    if(true) Future {throw new Exception("some artificial exception") }

    Future.sequence(resList.toList).map(_.sum)
  }


  /**
   * insert an ms run into the database.
   * @param run already parsed and ready
   * @return a Future of the same run (something else might be better)
   */
  def insert(run: MSRun): Future[Int] = {
    val enumerator = Enumerator(run.msnSpectra: _*)
    collection.bulkInsert(enumerator)
  }

  /**
   * remove all msnSpectra for a given run
   * @param runId the run id
   * @return
   */
  def delete(runId: Set[RunId]): Future[Boolean] = {
    val query = Json.obj("ref.spectrumId.runId" -> Json.obj("$in" -> runId.toList))
    collection.remove(query).map {
      case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
      case _ => true
    }
}

  /**
   * Returns just the spectra ref for a given run
   * @param runId the run id
   * @return
   */
  def findAllSpectraRefByrunId(runId: RunId): Future[Seq[SpectrumRef]] =findAllSpectraRefByrunId(Set(runId))

  /**
   * Returns just the spectra ref as a set of runIds
   * @param runIds the target set of runIds
   * @return
   */
  def findAllSpectraRefByrunId(runIds: Set[RunId]): Future[Seq[SpectrumRef]] = {
    val query = Json.obj("ref.spectrumId.runId" -> Json.obj("$in" -> runIds.map(_.value).toList))
    val projection = Json.obj("ref" -> 1, "_id" -> 0)

    collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map(lo => lo.map({ o =>
      Json.fromJson[SpectrumRef](o \ "ref").asOpt.get
    }))
  }

  /**
   * retrieves  by run & spectra title (unique by index setup)
   * @param runId the run id
   * @param title the spectrum title
   * @return
   */
  def findSpectrumByRunIdAndTitle(runId: RunId, title: String): Future[ExpMSnSpectrum] = {
    val query = Json.obj("ref.spectrumId.runId" -> runId.value, "ref.title" -> title)
    collection.find(query).cursor[ExpMSnSpectrum].headOption map {
      case Some(sp: ExpMSnSpectrum) => sp
      case None => throw new MongoNotFoundException(s"${runId.value}/$title")
    }
  }

  /**
   * retrieves  by SpectruRef (run & spectra id)
   * @param spId the spectrum reference
   * @return
   */
  def findSpectrumBySpId(spId: SpectrumId): Future[ExpMSnSpectrum] = {
    val query = Json.obj("ref.spectrumId.runId" -> spId.runId.value, "ref.spectrumId.id" -> spId.id.value)
    collection.find(query).cursor[ExpMSnSpectrum].headOption map {
      case Some(sp: ExpMSnSpectrum) => sp
      case None => throw new MongoNotFoundException(s"${spId.runId.value}/$spId.id.value")
    }
  }

  /**
   * retrieves  by run & scanNumber (unique by index setup)
   * @param runId the run id
   * @param spId the spectrum id
   * @return
   */
  def findSpectrumByRunIdAndScanNumber(runId: RunId, spId: SpectrumUniqueId): Future[ExpMSnSpectrum] = {
    val query = Json.obj("ref.spectrumId.runId" -> runId.value, "ref.spectrumId.id" -> spId.value)
    collection.find(query).cursor[ExpMSnSpectrum].headOption map {
      case Some(sp: ExpMSnSpectrum) => sp
      case None => throw new MongoNotFoundException(s"${runId.value}/$spId")
    }
  }

  /**
   * retrieves spectrum reference by run & scanNumber (unique by index setup)
   * @param runId the run id
   * @param spId the spectrum id
   * @return
   */
  def findSpectrumRefByRunIdAndScanNumber(runId: RunId, spId: SpectrumUniqueId): Future[SpectrumRef] = {
    val query = Json.obj("ref.spectrumId.runId" -> runId.value, "ref.spectrumId.id" -> spId.value)

    val projection = Json.obj("ref" -> 1)

    collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map({ lo =>
        val spList = lo.map({ o =>
          Json.fromJson[SpectrumRef](o \ "ref").asOpt.get
        })
        spList(0)
      })
  }


  /**
   * retrieves all spectra by run
   * @param runId the run id
   * @return
   */
  def findSpectrumByRunId(runId: RunId): Future[Iterator[ExpMSnSpectrum]] = {
    val query = Json.obj("ref.spectrumId.runId" -> runId.value)
    collection.find(query).cursor[ExpMSnSpectrum].collect[Iterator]()
  }

  /**
   * retrieve all MS2 spectra which have a precursor in the given range
   * @param runId
   * @param moz
   * @param daltonTolerance
   * @return
   */
  def findSpectrumByMozTol(runId:RunId, moz:Moz, daltonTolerance:Double): Future[Seq[ExpMSnSpectrum]] = {
    val lowerLimit = moz.value - daltonTolerance
    val upperLimit = moz.value + daltonTolerance
    val query = Json.obj(
      "ref.spectrumId.runId" -> runId.value,
      "ref.precursor.moz" -> Json.obj("$gte" -> lowerLimit, ("$lte" -> upperLimit))
    )

    collection.find(query).cursor[ExpMSnSpectrum].collect[Seq]()
  }

  /**
   * retrieve all spectrum info which have a precursor in the given range
   * @param runId
   * @param moz
   * @param daltonTolerance
   * @return
   */
  def findSpectrumRefByMozTol(runId:RunId, moz:Moz, daltonTolerance:Double): Future[Seq[SpectrumRef]] = {
    val lowerLimit = moz.value - daltonTolerance
    val upperLimit = moz.value + daltonTolerance
    val query = Json.obj(
      "ref.spectrumId.runId" -> runId.value,
      "ref.precursor.moz" -> Json.obj("$gte" -> lowerLimit, ("$lte" -> upperLimit))
    )

    val projection = Json.obj("ref" -> 1)

    collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map(lo => lo.map({ o =>
        Json.fromJson[SpectrumRef](o \ "ref").asOpt.get
      }))

  }

  /**
   * retrieve all spectrum info which have a precursor in the given range
   * @param runId
   * @param moz
   * @param charge
   * @param ppmTolerance
   * @return
   */
  def findSpectrumRefByMassTol(runId:RunId, moz:Moz, charge: Charge, ppmTolerance:Double): Future[Seq[SpectrumRef]] = {

    val mass: MolecularMass = CommonFunctions.computeMass(moz, charge)

    val daltonTolerance = mass.value / 1000000 * ppmTolerance

    val lowerLimit = mass.value - daltonTolerance
    val upperLimit = mass.value + daltonTolerance

    val query = Json.obj(
      "ref.spectrumId.runId" -> runId.value,
      "ref.precursor.molecularMass" -> Json.obj("$gte" -> lowerLimit, ("$lte" -> upperLimit))
    )

    val projection = Json.obj("ref" -> 1)

    val futureSpList:Future[Seq[SpectrumRef]] = collection.find(query, projection)
      .cursor[JsObject]
      .collect[List]()
      .map(lo => lo.map({ o =>
      Json.fromJson[SpectrumRef](o \ "ref").asOpt.get
    }))

    // check if they really correspond to this search
    futureSpList.map({ spList =>
      spList.filter(sp => CommonFunctions.spIsValid(sp, lowerLimit, upperLimit))
    })
  }

  /**
   * get the list of the run ids
   * @return
   */
  def listMsRunIds: Future[Seq[RunId]] = {
    val command = RawCommand(BSONDocument("distinct" -> collectionName, "key" -> "ref.spectrumId.runId"))
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
   * count the number of Spectra
   * @return
   */
  def countMsnSpectra: Future[Int] = {
    db.command(Count(collectionName))
  }

  /**
   * count the number of runs
   * @return
   */
  def countMsRuns: Future[Int] = {
    listMsRunIds.map(_.size)
  }


  /**
   * a maps with various counts (number of spectra, run ...)
   * @return
   */
  def stats: Future[Map[String, Int]] = {
    for {
      nSpectra <- countMsnSpectra
      nRuns <- countMsRuns
    } yield {
      Map("runs" -> nRuns, "msnSpectra" -> nSpectra)

    }
  }

  /**
    * find an entry by its spectrumId and set the given correctedMolMass information
    * @param spectrumId
    * @param correctedMolMass
    * @return
    */
  def findAndUpdateMolMass(spectrumId: SpectrumId, correctedMolMass: MolecularMass, molMassSource: String): Future[Int] = {
    val query = Json.obj(
      "ref.spectrumId.runId" -> spectrumId.runId.value,
      "ref.spectrumId.id" -> spectrumId.id.value
    )

    val update = Json.obj(
      "$set" -> Json.obj("ref.precursor.molecularMass" -> correctedMolMass.value),
      "$set" -> Json.obj("ref.precursor.molecularMassSource" -> molMassSource)
    )

    val future = collection.update(query,update)

    future.onFailure {
      case e => MongoNotFoundException(e.getMessage)
    }

    future.map({ le =>
      le.updated
    })

  }

}


object ExpMongoDBService extends Controller with MongoController {
  val default = new ExpMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default


}


