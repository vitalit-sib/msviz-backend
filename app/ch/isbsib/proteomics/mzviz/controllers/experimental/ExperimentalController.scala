package ch.isbsib.proteomics.mzviz.controllers.experimental

import java.io.File
import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime, Moz}
import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.importer._
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, MSRun, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models._
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1MySqlDBService, ExpMs1MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMs1TmpMongoDBService

import scala.concurrent.Future
import scala.util.{Failure, Success}
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.Play.current
import play.api.libs.json.Json
import play.api.libs.json.Json._

import scala.slick.jdbc.meta.MTable


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/exp", description = "experimental data access")
object ExperimentalController extends CommonController {

  implicit val ms1Format = Json.format[Ms1Peak]
  val ms1Dao = TableQuery[ExpMs1MySqlDBService]

  def setupMySqlTables = DBAction { implicit rs =>
    if(MTable.getTables("MS").list.isEmpty) {
      ms1Dao.ddl.create
      Ok("table MS was created")
    }else{
      Ok("table MS already existed")
    }
  }


//  @ApiOperation(nickname = "findXicMySql",
//    value = "find all ms1 for a given run id and moz in the MySQL database",
//    notes = """Returns only list of retention times and intensities""",
//    httpMethod = "GET")
//  @ApiImplicitParams(Array(
//    new ApiImplicitParam(name = "tolerance", value = "tolerance", required = false, dataType = "Double", paramType = "query"),
//    new ApiImplicitParam(name = "rtTolerance", value = "rtTolerance", required = false, dataType = "Double", paramType = "query")
//  ))
//  def findXicMySql(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String,
//              @ApiParam(value = """m/z""", defaultValue = "") @PathParam("moz") moz: Double,
//              tolerance: Option[Double]=None,
//              rtTolerance: Option[Double]=None
//               ) =
//    DBAction { implicit rs =>
//
//      // set the default value to 10 ppm
//      val ppmTolerance = tolerance.getOrElse(10.0)
//      val daltonTolerance = moz / 1000000 * ppmTolerance
//
//      val ms1List = ms1Dao.filter(ms => (ms.ref === runId)
//        && (ms.moz <= moz+daltonTolerance)
//        && ms.moz >= moz-daltonTolerance).list.map(m => Ms1EntryWithRef(RunId(m.ref), RetentionTime(m.rt), Intensity(m.int), Moz(m.moz))
//      )
//
//      val sphList = ExpMs1MongoDBService().extract2Lists(ms1List, rtTolerance.getOrElse(10.0))
//
//      Ok(sphList)
//    }


  @ApiOperation(nickname = "loadMS1MySql",
    value = "Loads a mzxml file to mysql database",
    notes = """ source will be a unique descriptor on the source""",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "mzxml", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadMS1MySql(@ApiParam(name = "runId", value = "a string id with run identifier", required = true) @PathParam("runId") runId: String) =
    DBAction(parse.temporaryFile) { implicit rs =>
        val entries = FastLoaderMzXML.parseFile(rs.request.body.file, RunId(runId))

        val nrInserted = entries.map({ e =>
          val rt = e.retentionTime.value
          val runId = e.spId.runId.value

          val ms1Peaks = e.peaks.map({ peak =>
            Ms1Peak(runId, rt, peak.moz.value, peak.intensity.value)
          })

          // insert peaks into MySql
          ms1Dao ++= ms1Peaks
          ms1Peaks.length
        }).sum

      Ok(Json.obj("inserted" -> nrInserted.toString))
    }


  def stats = Action.async {
    ExpMongoDBService().stats.map { st =>
      Ok(jsonWritesMap.writes(st))
    }
  }

  @ApiOperation(nickname = "listMSRunIds",
    value = "the list of run ids",
    notes = """from the parameter run-id at load time""",
    response = classOf[List[String]],
    httpMethod = "GET")
  def listMSRunIds = Action.async {
    ExpMongoDBService().listMsRunIds.map {
      ids => Ok(Json.toJson(ids.map(_.value)))
    }
  }

  @ApiOperation(nickname = "loadMSRun",
    value = "Loads an MGF peak list as a run",
    notes = """ runId will be important to link with the mzid""",
    response = classOf[String],
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "mgf peak list", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadMSRun(@ApiParam(name = "runId", value = "a string id with run identifier", required = true) @PathParam("runId") runId: String) = Action.async(parse.temporaryFile) {
    request =>

      LoaderMGF.load(request.body.file, RunId(runId)) match {
        case Success(it) =>{

          var i = 0

          while(it.hasNext){
            val msnRun= new MSRun(RunId(runId), Seq(it.next))
            ExpMongoDBService().insert(msnRun)
            i += 1
          }

          Future {
            Ok(Json.obj("inserted" -> i))
          }

        }.recover {
          case e => BadRequest(e.getMessage)
        }
        case Failure(e) => Future {
          BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }
      }

  }

  @ApiOperation(nickname = "findExpSpectrum",
    value = "find a spectrum by run id and spectrum title",
    notes = """the tuple should be unique by indexing""",
    response = classOf[ExpMSnSpectrum],
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "sortByMoz", value = "sort the fragment peaks by m/z (defaukt false)", required = false, dataType = "Boolean", paramType = "query"),
    new ApiImplicitParam(name = "mostIntense", value = "take the n most intense peaks", required = false, dataType = "Integer", paramType = "query")
  ))
  def findExpSpectrum(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String,
                      @ApiParam(value = """spectrum id""", defaultValue = "") @PathParam("spId") spId: String,
                      sortByMoz: Option[Boolean]=None,
                      mostIntense: Option[Integer]=None
                       ) =
    Action.async {
      ExpMongoDBService().findSpectrumByRunIdAndScanNumber(RunId(runId),SpectrumUniqueId(spId))
        .map { case sp: ExpMSnSpectrum =>
        val peaks = (sortByMoz match {
          case Some(false) => sp.peaks
          case _ => sp.peaks.sortBy(_.moz.value)
        }).filter({ p =>
          mostIntense match {
            case None => true
            case Some(thres) =>
              p.intensityRank.value <= thres
          }
        })
          Ok(Json.toJson(ExpMSnSpectrum(sp.ref,peaks)))
      }
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  @ApiOperation(nickname = "findAllSpectraRefByRunId",
    value = "find all spectra for a given run id",
    notes = """Returns only the reference information (precursor & co)""",
    response = classOf[List[SpectrumRef]],
    httpMethod = "GET")
  def findAllSpectraRefByRunId(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String) =
    Action.async {
      ExpMongoDBService().findAllSpectraRefByrunId(RunId(runId))
        .map { case sphList: List[JsObject] => Ok(Json.toJson(sphList)) }
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
      }
    }

  // val ms1Dao = TableQuery[ExpMs1MySqlDBService]

  @ApiOperation(nickname = "deleteMSRun",
    value = "delete all ms1 and ms2+ spectra",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteMSRun(runIds: String) = Action.async {

    val runIdSet = runIds.split(",").map(RunId(_)).toSet


    // delete ms1 spectra
    val ms1Del: Int = DB.withSession { implicit session =>
        runIdSet.map({ runId =>
          ms1Dao.filter(ms => (ms.ref === runId.value)).delete
        }).sum
      }

    for{
    // delete ms2+ spectra
      msnDel <- ExpMongoDBService().delete(runIdSet)
    }yield{
      Ok(Json.obj("msn" -> msnDel, "ms1" -> ms1Del))
    }

  }


  @ApiOperation(nickname = "findXIC",
    value = "find all ms1 for a given run id and moz",
    notes = """Returns only list of retention times and intensities""",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "tolerance", value = "tolerance", required = false, dataType = "Double", paramType = "query"),
    new ApiImplicitParam(name = "rtTolerance", value = "rtTolerance", required = false, dataType = "Double", paramType = "query")
  ))
  def findXic(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String,
              @ApiParam(value = """m/z""", defaultValue = "") @PathParam("moz") moz: Double,
              tolerance: Option[Double]=None,
             rtTolerance: Option[Double]=None
               ) =
    Action.async {

      // set the default value to 10 ppm
      val ppmTolerance = tolerance.getOrElse(10.0)
      val daltonTolerance = moz / 1000000 * ppmTolerance

      Future{Ok("hoho")}
//      val futureList = ExpMs1BinMongoDBService().findMs1EntryWithMozTol(RunId(runId),Moz(moz),daltonTolerance)
//
//      ExpMs1MongoDBService().extract2FutureLists(futureList, rtTolerance.getOrElse(1.0))
//            .map { case sphList: JsObject => Ok(sphList) }
//              .recover {
//              case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
//            }
    }


  @ApiOperation(nickname = "loadMS1",
    value = "Loads a mzxml file",
    notes = """ source will be a unique descriptor on the source""",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "mzxml", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadMS1(@ApiParam(name = "runId", value = "a string id with run identifier", required = true) @PathParam("runId") runId: String, intensityThreshold: Double = 1000) =
    Action.async(parse.temporaryFile) {
      request =>

        val ms1SpIter: Iterator[ExpMs1Spectrum] = LoaderMzML().parse(request.body.file, RunId(runId)).filter(_.isLeft).map(_.left.get)

        (for {
          // insert all peaks above threshold into a temporary mongodb collection
          resMs1Insertion <- ExpMs1TmpMongoDBService().insertMs1spectra(ms1SpIter, intensityThreshold)
        } yield {
            Ok(Json.obj("nrMs1Peaks" -> resMs1Insertion))

        }).recover{
            case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }

    }


  @ApiOperation(nickname = "options",
    value = "empty options method",
    notes = """returns Ok to fulfill the pre-flight OPTIONS request""",
    response = classOf[String],
    httpMethod = "OPTIONS")
  def options(@ApiParam(name = "runId", value = "a string id with run identifier", required = true) @PathParam("runId") runId: String) =
    Action {
      Ok("Ok")
    }

}
