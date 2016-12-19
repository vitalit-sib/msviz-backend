package ch.isbsib.proteomics.mzviz.uploads

import java.lang
import java.util.Calendar

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.commons.importers.ImporterException
import ch.isbsib.proteomics.mzviz.controllers.experimental.ExperimentalController._
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, ExpMs1Spectrum}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.{LoaderMGF, LoaderMzML}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.matches.models.SubmissionStatus
import ch.isbsib.proteomics.mzviz.matches.services.{CommonMatchService, SearchInfoDBService, ProteinMatchMongoDBService, MatchMongoDBService}
import ch.isbsib.proteomics.mzviz.uploads.LoaderMQData._
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import scala.util.control.NonFatal
import play.api.Logger

import scala.concurrent.Future
import java.nio.file.Files
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File

import scala.util.{Try, Failure}
import scala.xml.Elem

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMascotData(val db: DefaultDB) {

  // file types that are required
  val requiredTypes = Set("mzid", "mgf", "mzml")

  // define services
  val ms1Service = new ExpMs1BinMongoDBService(db)
  val msnService = new ExpMongoDBService(db)
  val matchService = new MatchMongoDBService(db)
  val protMatchService = new ProteinMatchMongoDBService(db)
  val searchInfoService = new SearchInfoDBService(db)
  val commonMatchService = new CommonMatchService(db)

  /**
   * load a zip file containing a run per subfolder
   *
   * @param zipFile
   * @return
   */
  def loadZip(zipFile: File, intensityThreshold: Double): Future[Seq[SearchId]] = {

    val unzipPath:Try[String] = Try ( FileFinder.getHighestDir(Unzip.unzip(zipFile)) )

    // create a searchId with error if unzip fails
    unzipPath.recover({
      case NonFatal(e) => {
        val errorMessage = s"Could not read ZIP file."
        val now = Calendar.getInstance().getTime()
        searchInfoService.createSearchIdWithError(SearchId(now.toString), errorMessage)
        Logger.error(errorMessage)
        Future{ throw new ImporterException(errorMessage, e) }
      }
    })

    val loadingResult:Future[Seq[SearchId]] = loadUnzipped(unzipPath.get, intensityThreshold)

    loadingResult
  }

  /**
   * load all data from the path
   *
   * @param path
   * @param intensityThreshold
   * @return
   */
  def loadUnzipped(path: String, intensityThreshold: Double): Future[Seq[SearchId]] = {

    // get the list of files in the directory
    val fileList = FileFinder.getListOfFiles(path)

    // keep mzId files
    val mzIdFiles:List[File] = (fileList.filter(x => x.getName.split("\\.").last.toLowerCase() == "mzid"))

    //if there are no mzIdFiles then send error
    if (mzIdFiles.isEmpty){
      val errorMessage = "MzIdentML files not found. Verify if your Mascot data is correct"
      val now = Calendar.getInstance().getTime()
      searchInfoService.createSearchIdWithError(SearchId(now.toString), errorMessage)
      Logger.error(errorMessage)
      Failure(new Exception(errorMessage))
    }

    // list of searchIds
    val searchIds = mzIdFiles.map(x => SearchId("MSC_" + x.getName.split("\\.")(0)))

    // get the XML parsers
    val mzMlXmlElems:Try[List[Elem]] = Try(mzIdFiles.map(file => scala.xml.XML.loadFile(file)))

      // get the corresponding mzML files
      // zip XML elements and mzid names, List[(Elem, File)]
      val mzMlFiles:Try[List[File]] = mzMlXmlElems.map(_.zip(mzIdFiles).map({
        tuple =>
          val found= LoaderMzIdent.parseSpectraFilename(tuple._1)
          //if resubmitted job, SpectraData location file is empty, so we take mzid name
          val  filename=
            if(found == "") path + "/" + tuple._2.toString.split("\\/").last.split("\\.")(0)
            else path + "/" + found

          new File(filename + ".mzML")
      })).recoverWith({
        case NonFatal(e) => {
          val now = Calendar.getInstance().getTime()
          val errorMessage = "There must be something wrong with one of your MzIdentMl files."
          Logger.error(errorMessage)
          searchInfoService.createSearchIdWithError(SearchId(now.toString), errorMessage)
          Failure(e)
        }
      })

    val allMzMlFound = checkMzMlFilesExist(mzMlFiles.get, searchIds)

    // if not all mzML files are available we throw an exception
    if(allMzMlFound){
      // check if searchIds are already taken
      val searchIdAlreadyExists = checkSearchIdsExists(searchIds)
      searchIdAlreadyExists.flatMap(alreadyExists => {
        if (alreadyExists._1){
          val searchIdsString = searchIds.map(_.value).reduceLeft(_ + "," + _)
          val errorMessage = s"Some of the SearchIds [$searchIdsString] already exist."
          Logger.error(errorMessage)
          throw new ImporterException(errorMessage)
        }
        else insertAllData(searchIds, mzIdFiles, mzMlFiles.get, mzMlXmlElems.get, intensityThreshold)
      })
    }
    else {
      val errorMessage = s"There are mzMl files missing in the given zip file: [$path]"
      Logger.error(errorMessage)
      throw new ImporterException(errorMessage)
    }

  }

  /**
   * Check if any of the given SearchIds already exists.
   * In case it already exists we create an error SearchId for the given SearchId
   * @param searchIds
   * @return
   */
  def checkSearchIdsExists(searchIds: Seq[SearchId]):Future[(Boolean, List[SearchId])] = {
    // assert that searchIds are not already inserted
    val searchIdCheck:Seq[Future[(Boolean, SearchId)]] = searchIds.map({ id =>
      searchInfoService.isSearchIdExist(id).map({ alreadyTaken =>
        if(alreadyTaken) searchInfoService.createSearchIdWithError(id, s"SearchId [${id.value}] already exists. Please delete SearchIds with this name before reloading")
        (alreadyTaken, id)
      })
    })

    // check if all searchIds are ok
    val failedSearchIds:List[SearchId] = List()

    val searchIdAlreadyExists:Future[(Boolean, List[SearchId])] = Future.sequence(searchIdCheck).map({
      found => found.foldLeft((false, failedSearchIds))({(a,b) =>
        ((a._1 | b._1), if(b._1) b._2 :: a._2 else a._2)
      })
    })

    searchIdAlreadyExists
  }


  /**
   * Check if the given mzMl files exist.
   * If they are missing we create the corresponding SearchIds with errors
   * @param mzMlFiles
   * @param searchIds
   * @return
   */
  def checkMzMlFilesExist(mzMlFiles: Seq[File], searchIds: Seq[SearchId]): Boolean = {
    // assert that all mzML files are here
    val filesFound = mzMlFiles.zip(searchIds).map({ case (mzMlFile,searchId) =>
      if (! Files.exists(mzMlFile.toPath)) {
        val errorMessage = s"Error while parsing [${searchId.value}]. Could not find mzML file [${mzMlFile.getName}]"
        Logger.error(errorMessage)
        searchInfoService.createSearchIdWithError(searchId, errorMessage)
        false
      } else true
    })

    filesFound.reduceLeft(_ & _)
  }


  /**
   * insert all data to the database
   *
   * @param searchIds
   * @param mzIdFiles
   * @param mzMlFiles
   * @param mzMlXmlElems
   * @param intensityThreshold
   * @return
   */
  def insertAllData(searchIds: List[SearchId],
                    mzIdFiles: List[File],
                    mzMlFiles: List[File],
                    mzMlXmlElems: List[Elem],
                    intensityThreshold:Double): Future[Seq[SearchId]] = {

    // the callback to update the searchId status
    def updateStatus(searchId:SearchId, code: String, message:String) = {
      Logger.info(s"changed [${searchId.value}] to status [$code] with message: $message")
      val status = new SubmissionStatus(code=code, message = message)
      searchInfoService.updateStatus(searchId, status)
    }

    val insertedIds = for{
      nrMatch <- insertMatchData(mzIdFiles.zip(searchIds.zip(mzMlXmlElems)), Some(updateStatus))
      nrExp <- insertExpData(mzMlFiles.zip(searchIds), intensityThreshold, Some(updateStatus))
    } yield {
      //nrExp + nrMatch
      searchIds
    }

    // if one SearchId fails we remove all SearchIds
    insertedIds.recover({
      case NonFatal(e) => {
        searchIds.foreach({ id =>
          commonMatchService.deleteAllMatchInfo(id)
          ms1Service.deleteAllByRunIds(Set(RunId(id.value)))
          msnService.delete(Set(RunId(id.value)))
        })
        val errorMessage = "Error while loading experimental data. " + e.getMessage
        Logger.error(errorMessage)
        throw new ImporterException(errorMessage, e)
      }
    })

  }


  /**
   * insert psm, proteinInfo and searchInfo data from a list of mzId files
   *
   * @param mzIdFiles
   * @param updateStatusCallback
   * @return
   */
  def insertMatchData(mzIdFiles: List[(File, (SearchId, Elem))],
                      updateStatusCallback: Option[(SearchId, String, String) => Future[Boolean]] = None): Future[Int] = {
    // insert one by one
    mzIdFiles.foldLeft(Future{0})( (futureA, b) =>
      for {
        a <- futureA
        c <- insertOneMatch(b._1, b._2, updateStatusCallback).get
      } yield {
        a + c
      })

  }

  /**
   * insert psm, proteinInfo and searchInfo data from one mzId file
   *
   * @param mzIdFile
   * @param searchId
   * @param updateStatusCallback
   * @return
   */
  def insertOneMatch(mzIdFile: File,
                     searchId: (SearchId, Elem),
                     updateStatusCallback: Option[(SearchId, String, String) => Future[Boolean]] = None): Try[Future[Int]] = {

    Try {

      val matchData = LoaderMzIdent.parseWithXmlElem(mzIdFile, searchId._1, RunId(searchId._1.value), searchId._2, Some("Mascot"))

      for {
      // and only last the other data
        psmNr <- matchService.insert(matchData._1)
        proteinNumber <- protMatchService.insert(matchData._2)
        searchOk <- searchInfoService.insert(matchData._3)

      } yield {
        if (updateStatusCallback.isDefined) updateStatusCallback.get.apply(searchId._1, "loading", "waiting to load experimental data")
        psmNr + proteinNumber + (if (searchOk) 1 else 0)
      }

    }.recoverWith({
      case NonFatal(e) => {
        val errorMessage = s"There is something wrong with your MzIdentMl file [${mzIdFile.getName}]"
        Logger.error(errorMessage)
        searchInfoService. createSearchIdWithError(searchId._1, errorMessage)
        Failure(new ImporterException(errorMessage, e))
      }
    })

  }


  /**
   * insert ms1 and ms2 data from a list of mzML files
   *
   * @param mzMlFiles
   * @param intensityThreshold
   * @param updateStatusCallback
   * @return
   */
  def insertExpData(mzMlFiles: List[(File, SearchId)],
                    intensityThreshold: Double,
                    updateStatusCallback: Option[(SearchId, String, String) => Future[Boolean]] = None): Future[Int] = {

    // insert one by one
    mzMlFiles.foldLeft(Future{0})( (futureA, b) =>
      for {
        a <- futureA
        c <- insertOneExp(b._1, b._2, intensityThreshold, updateStatusCallback)
      } yield {
        a + c
      })
  }

  /**
   * insert ms1 and ms2 data from one mzML file
   * @param mzMlFile
   * @param id
   * @param intensityThreshold
   * @param updateStatusCallback
   * @return
   */
  def insertOneExp(mzMlFile: File,
                   id: SearchId,
                   intensityThreshold: Double,
                   updateStatusCallback: Option[(SearchId, String, String) => Future[Boolean]] = None): Future[Int] = {

    val itMs1Ms2 = Try( LoaderMzML().parse(mzMlFile, RunId(id.value)) ).recoverWith({
      case NonFatal(e) => {
        val errorMessage = s"Error while parsing MzML file. There is something wrong with MzML file [${mzMlFile.getName}]"
        Logger.error(errorMessage)
        searchInfoService.createSearchIdWithError(id, errorMessage)
        Failure(new ImporterException(errorMessage, e))
      }
    })

    val itMs1Ms2parts = itMs1Ms2.get.partition(_.isLeft)

    val itMs1: Iterator[ExpMs1Spectrum] = itMs1Ms2parts._1.map(_.left.get)
    val itMs2: Iterator[ExpMSnSpectrum] = itMs1Ms2parts._2.map(_.right.get)

    updateStatusCallback.get.apply(id, "loading", "loading ms1 data")

    val insertMs1:Future[Boolean] = ms1Service.insertMs1spectra(itMs1, intensityThreshold)

    insertMs1.recover({
      case NonFatal(e) => {
        val errorMessage = s"Error while loading ms1 data."
        Logger.error(errorMessage)
        searchInfoService.createSearchIdWithError(id, errorMessage)
        throw new ImporterException(errorMessage, e)
      }
    })

    insertMs1.flatMap({ ms1Inserted =>
      if(updateStatusCallback.isDefined){
        updateStatusCallback.get.apply(id, "loading", "loading ms2 data")
      }

      val insertMs2: Future[Int] = msnService.insertMs2spectra(itMs2, RunId(id.value)).recover({
        case NonFatal(e) => {
          val errorMessage = s"Error while loading ms2 data."
          searchInfoService.createSearchIdWithError(id, errorMessage)
          throw new ImporterException(errorMessage, e)
        }
      })

      insertMs2.map({ ms2Inserted =>
        if(updateStatusCallback.isDefined){
          updateStatusCallback.get.apply(id, "done", "finished ms2 load")
        }

        (if(ms1Inserted) 0 else 1) + ms2Inserted
      })
    })

  }
}


/**
 * the companion object
 */

object LoaderMascotData extends Controller with MongoController {

  val default = new LoaderMascotData(db)

  def apply() = default

}
