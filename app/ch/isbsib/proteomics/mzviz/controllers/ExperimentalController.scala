package ch.isbsib.proteomics.mzviz.controllers

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.IdRun
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.services.ExpMongoDBService
import play.api.libs.Files
import play.api.mvc.{MultipartFormData, Request, Action, Controller}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.concurrent.Future
import scala.util.{Success, Try}

// Reactive Mongo imports

import reactivemongo.api._

// Reactive Mongo plugin, including the JSON-specialized collection

import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

/**
 * @author Alexandre Masselot
 */
object ExperimentalController extends Controller {

  def stats = Action {
    Ok("yo") //Json.obj(ExpMongoDBService.stats))
  }

  private def localFile(paramName: String, request: Request[MultipartFormData[Files.TemporaryFile]]): Future[Tuple2[File, String]] = {
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

  def listMSRunIds = Action.async {
    ExpMongoDBService().listMsRunIds.map {
      ids => Ok(Json.obj("msRuns" -> ids.map(_.value)))
    }

  }

  def loadMSRun = Action.async(parse.multipartFormData) {
    request =>
      localFile("mgf", request).map({
        case (uploadedFile, filename) =>
          val idRun = request.body.dataParts.get("run-id").map(_.head)
          val msRun = LoaderMGF.load(uploadedFile.getAbsolutePath, idRun)
          ExpMongoDBService().insert(msRun)
          Ok
      })
  }

  def deleteMSRun(id: String) = Action.async {
    ExpMongoDBService().delete(IdRun(id)).map { x=>
      Ok("OK")
    }


  }
}
