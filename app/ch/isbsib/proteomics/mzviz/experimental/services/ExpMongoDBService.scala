package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.{IntensityRank, Intensity, Moz, MSLevel}
import ch.isbsib.proteomics.mzviz.commons.services.{MongoDBService, MongoNotFoundException}
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMs1Spectrum, ExpPeakMSn, ExpMSnSpectrum, SpectrumRef}
import ch.isbsib.proteomics.mzviz.experimental.{ScanNumber, RunId, MSRun}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
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
  // peak array are serilized for sake of speed
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
  def delete(runId: RunId): Future[Boolean] = {
    val query = Json.obj("ref.spectrumId.runId" -> runId.value)
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
   * Returns just the spectra ref fin a set of runIds
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
   * retrieves  by run & scanNumber (unique by index setup)
   * @param runId the run id
   * @param scanNumber the scan Number
   * @return
   */
  def findSpectrumByRunIdAndScanNumber(runId: RunId, scanNumber: Int): Future[ExpMSnSpectrum] = {
    val query = Json.obj("ref.spectrumId.runId" -> runId.value, "ref.spectrumId.id" -> scanNumber)
    println(query)
    collection.find(query).cursor[ExpMSnSpectrum].headOption map {
      case Some(sp: ExpMSnSpectrum) => sp
      case None => throw new MongoNotFoundException(s"${runId.value}/$scanNumber")
    }
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
}


object ExpMongoDBService extends Controller with MongoController {
  val default = new ExpMongoDBService(db)

  /**
   * get the default database
   * @return
   */
  def apply() = default


}


