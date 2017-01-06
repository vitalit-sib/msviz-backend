package ch.isbsib.proteomics.mzviz.controllers.experimental

import java.io.File
import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.controllers.CommonController
import ch.isbsib.proteomics.mzviz.controllers.JsonCommonsFormats._
import ch.isbsib.proteomics.mzviz.experimental.importer._
import ch.isbsib.proteomics.mzviz.experimental.{MSRun, RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.models._
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import com.wordnik.swagger.annotations._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Action
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMs1BinMongoDBService

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import play.api.Play.current
import play.api.libs.json.Json
import play.api.libs.json.Json._



/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/exp", description = "experimental data access")
object ExperimentalController extends CommonController {


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

      Try{
        val ms1Iterator = LoaderMGF.load(request.body.file, RunId(runId))
        ExpMongoDBService().insertMs2spectra(ms1Iterator, RunId(runId))
      } match {
        case Success(insertedNr) => {

          insertedNr.map(nr => Ok(Json.obj("inserted" -> nr)))

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

  @ApiOperation(nickname = "findSpectrumRef",
    value = "find a spectrum reference by run id and spectrum title",
    notes = """the tuple should be unique by indexing""",
    response = classOf[SpectrumRef],
    httpMethod = "GET")
  def findSpectrumRef(@ApiParam(value = """run id""", defaultValue = "") @PathParam("runId") runId: String,
                      @ApiParam(value = """spectrum id""", defaultValue = "") @PathParam("spId") spId: String
                       ) =
    Action.async {
      ExpMongoDBService().findSpectrumRefByRunIdAndScanNumber(RunId(runId), SpectrumUniqueId(spId))
        .map { case spref: SpectrumRef => Ok(Json.toJson(spref)) }
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


  @ApiOperation(nickname = "deleteMSRun",
    value = "delete all ms1 and ms2+ spectra",
    notes = """No double check is done. Use with caution""",
    response = classOf[String],
    httpMethod = "DELETE")
  def deleteMSRun(runIds: String) = Action.async {

    val runIdSet = runIds.split(",").map(RunId(_)).toSet

    for{
    // delete ms2+ and ms1 spectra
      msnDel <- ExpMongoDBService().delete(runIdSet)
      ms1Del <- ExpMs1BinMongoDBService().deleteAllByRunIds(runIdSet)
    }yield{
      Ok(Json.obj("msn" -> msnDel, "ms1" -> ms1Del))
    }

  }



  @ApiOperation(nickname = "findSpectrumByMozTol",
    value = "find all ms2 for a given run id which have a precursor around moz with the given tolerance",
    notes = """Returns for ms1 list of retention times and intensities""",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "tolerance", value = "tolerance", required = false, dataType = "Double", paramType = "query")
  ))
  def findSpectrumByMozTol(@ApiParam(value = """run id""", required = true) @PathParam("runId") runId: String,
              @ApiParam(value = """m/z""", required = true) @PathParam("moz") moz: Double,
              @ApiParam(name = "tolerance", value = """the moz tolerance in ppm""", defaultValue = "10", required=false) @PathParam("tolerance") tolerance: Option[Double]=None
               ) =
    Action.async {

      // set the default value to 10 ppm
      val ppmTolerance = tolerance.getOrElse(10.0)
      val daltonTolerance = moz / 1000000 * ppmTolerance

      // and the corresponding Ms2 precursors
      val futureMs2List = ExpMongoDBService().findSpectrumByMozTol(RunId(runId), Moz(moz), daltonTolerance)

      futureMs2List.map {ms2List  => Ok(Json.toJson(ms2List)) }
        .recover {
          case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }

    }

  @ApiOperation(nickname = "findSpectrumRefByMozTol",
    value = "find all ms2 information for a given run id which have a precursor around moz with the given tolerance",
    notes = """Returns for ms1 list of retention times and intensities""",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "tolerance", value = "tolerance", required = false, dataType = "Double", paramType = "query")
  ))
  def findSpectrumRefByMozTol(@ApiParam(value = """run id""", required = true) @PathParam("runId") runId: String,
                           @ApiParam(value = """m/z""", required = true) @PathParam("moz") moz: Double,
                           @ApiParam(name = "tolerance", value = """the moz tolerance in ppm""", defaultValue = "10", required=false) @PathParam("tolerance") tolerance: Option[Double]=None
                            ) =
    Action.async {

      // set the default value to 10 ppm
      val ppmTolerance = tolerance.getOrElse(10.0)
      val daltonTolerance = moz / 1000000 * ppmTolerance

      // and the corresponding Ms2 precursors
      val futureMs2List = ExpMongoDBService().findSpectrumRefByMozTol(RunId(runId), Moz(moz), daltonTolerance)

      futureMs2List.map {ms2List  => Ok(Json.toJson(ms2List)) }
        .recover {
          case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }

    }

  @ApiOperation(nickname = "findSpectrumRefByMass",
    value = "find all ms2 information for a given run id which have a precursor around mass with the given tolerance",
    notes = """Returns for ms1 list of retention times and intensities""",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "tolerance", value = "tolerance", required = false, dataType = "Double", paramType = "query")
  ))
  def findSpectrumRefByMassTol(@ApiParam(value = """run id""", required = true) @PathParam("runId") runId: String,
                              @ApiParam(value = """m/z""", required = true) @PathParam("moz") moz: Double,
                               @ApiParam(value = """charge""", required = true) @PathParam("charge") charge: Int,
                              @ApiParam(name = "tolerance", value = """the mass tolerance in ppm""", defaultValue = "10", required=false) @PathParam("tolerance") tolerance: Option[Double]=None
                               ) =
    Action.async {

      // set the default value to 10 ppm
      val ppmTolerance = tolerance.getOrElse(10.0)

      // and the corresponding Ms2 precursors
      val futureMs2List = ExpMongoDBService().findSpectrumRefByMassTol(RunId(runId), Moz(moz), Charge(charge), ppmTolerance)

      futureMs2List.map {ms2List  => Ok(Json.toJson(ms2List)) }
        .recover {
        case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
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
  def findXic(@ApiParam(value = """run id""", required = true) @PathParam("runId") runId: String,
              @ApiParam(value = """m/z""", required = true) @PathParam("moz") moz: Double,
              @ApiParam(name = "tolerance", value = """the moz tolerance in ppm""", defaultValue = "10", required=false) @PathParam("tolerance") tolerance: Option[Double]=None,
              @ApiParam(name = "rtTolerance", value = """additional 0 intensity peaks are inserted if space between peaks is bigger than this tolerance""", defaultValue = "10") @PathParam("rtTolerance") rtTolerance: Option[Double]=None
               ) =
    Action.async {

      // set the default value to 10 ppm
      val ppmTolerance = tolerance.getOrElse(10.0)
      val daltonTolerance = moz / 1000000 * ppmTolerance

      val futureList = ExpMs1BinMongoDBService().findMs1EntryWithMozTol(RunId(runId),Moz(moz),daltonTolerance)

      val sphList = ExpMs1BinMongoDBService().extract2Lists(futureList, rtTolerance.getOrElse(10.0))

      sphList.map { case sphList: JsObject => Ok(sphList) }
        .recover {
          case e => BadRequest(e.getMessage + e.getStackTrace.mkString("\n"))
        }

    }


  @ApiOperation(nickname = "loadMS1",
    value = "Loads a mzxml file",
    notes = """ source will be a unique descriptor on the source""",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "body", value = "mzxml", required = true, dataType = "text/plain", paramType = "body")
  ))
  def loadMS1(@ApiParam(name = "runId", value = "a string id with run identifier", required = true) @PathParam("runId") runId: String,
              @ApiParam(name = "intensityThreshold", value = "ignore all peaks below this threshold", required = false) @PathParam("intensityThreshold") intensityThreshold: Option[Double] = None,
              @ApiParam(name = "fileType", value = "source of results: either MzML (default) or MzXML", required = true) @PathParam("fileType") fileType: Option[String] = None) =
    Action.async(parse.temporaryFile) {
      request =>

        // default values
        val intThres = intensityThreshold.getOrElse(1.0)
        val selType = fileType.getOrElse("MzML")

        val ms1SpIter: Iterator[ExpMs1Spectrum] = if(selType == "MzML"){
          LoaderMzML().parse(request.body.file, RunId(runId)).filter(_.isLeft).map(_.left.get)
        }else{
          LoaderMzXML().parse(request.body.file, RunId(runId))
        }

        (for {
          // insert all peaks above threshold into a temporary mongodb collection
          resMs1Insertion <- ExpMs1BinMongoDBService().insertMs1spectra(ms1SpIter, intThres)
        } yield {
            Ok(Json.obj("insertionFinished" -> resMs1Insertion))

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
