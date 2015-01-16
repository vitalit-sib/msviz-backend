package ch.isbsib.proteomics.mzviz.controllers

import java.io.File
import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, SpectrumRef}
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import com.wordnik.swagger.annotations._
import play.api.libs.Files
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.{Action, Controller, MultipartFormData, Request}

import scala.concurrent.Future


/**
 * Commons functionalities for our controllers
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/exp", description = "experimental data access")
trait CommonController extends Controller {


  def localFile(paramName: String, request: Request[MultipartFormData[Files.TemporaryFile]]): Future[Tuple2[File, String]] = {
    Future {
      val reqFile = request.body.file(paramName).get
      val filename = reqFile.filename
      val fTmp = File.createTempFile(new File(filename).getName + "-", ".local")
      fTmp.delete()
      fTmp.deleteOnExit()
      reqFile.ref.moveTo(fTmp)
      (fTmp, filename)
    }
  }

}
