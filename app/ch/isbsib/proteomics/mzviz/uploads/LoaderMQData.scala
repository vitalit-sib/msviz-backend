package ch.isbsib.proteomics.mzviz.uploads

import java.io.File
import java.nio.file.{Paths, Path, Files}
import java.util.Calendar

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.commons.importers.ImporterException
import ch.isbsib.proteomics.mzviz.controllers.matches.SearchController._
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMzML
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, ExpMs1Spectrum}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.{LoaderMaxQuant}
import ch.isbsib.proteomics.mzviz.matches.models.{SearchInfo, ProteinIdent, PepSpectraMatch, SubmissionStatus}
import ch.isbsib.proteomics.mzviz.matches.services.{CommonMatchService, SearchInfoDBService, ProteinMatchMongoDBService, MatchMongoDBService}
import ch.isbsib.proteomics.mzviz.results.basket.BasketMongoDBService
import play.api.libs.json.Json
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source._
import scala.util.{Failure, Try}
import scala.util.control.NonFatal
import scala.xml.Elem


/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMQData(val db: DefaultDB) {

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
   * load a zip file containing mzML and txt folder with MQ results
   *
   * @param zipFile
   * @return
   */
  def loadZip(zipFile: File, intensityThreshold: Double): Future[Seq[SearchId]] = {
    // unzip the file
    val unzipPath: Try[String] = Try(FileFinder.getHighestDir(Unzip.unzip(zipFile)))

    // create a searchId with error if unzip fails
    unzipPath.recover({
      case NonFatal(e) => {
        val errorMessage = s"Could not read ZIP file."
        val now = Calendar.getInstance().getTime()
        searchInfoService.createSearchIdWithError(SearchId(now.toString), errorMessage)
        Future {
          throw new ImporterException(errorMessage, e)
        }
      }
    })

    // goto highest path but without the txt
    val innerPath = unzipPath.get.split("\\/").dropRight(1).mkString("/")

    val loadingResult: Future[Seq[SearchId]] = loadUnzipped(innerPath, intensityThreshold)

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

    //parse txt/summary to obtain check if we have all expected files
    val summaryFile = path + "/txt/summary.txt"
    //Check if summary file exists

    if (!Files.exists(Paths.get(summaryFile))){
      val errM="No summary table detected. Check if you are loading MaxQuant data."
      val now = Calendar.getInstance().getTime()
      searchInfoService.createSearchIdWithError(SearchId(now.toString), errM)
      throw new Exception (errM)
    }

    // returns table with MzML and SearchID
    val summaryHash = Try(LoaderMaxQuant.parseMaxquantSummaryTableRawSearchId(new File(summaryFile)))

    // list of searchIds
    val searchIds: Try[List[SearchId]] = summaryHash.map(_.values.map {
      searchId => SearchId("MXQ_" + searchId.toString)
    }.toList).recoverWith({
      case NonFatal(e) => {
        val now = Calendar.getInstance().getTime()
        searchInfoService.createSearchIdWithError(SearchId(now.toString), "There must be something wrong with your summary table for the experiment. MsViz does not accept empty, repeated or values starting with numbers.")
        Failure(e)
      }
    })

    // get the corresponding mzML files
    val mzMlFiles: Try[List[File]] = summaryHash.map(_.keys.map {
      key =>
        val fileToFind = path + "/" + key + ".mzML"
        new File(fileToFind)
    }.toList).recoverWith({
      case NonFatal(e) => {
        val now = Calendar.getInstance().getTime()
        searchInfoService.createSearchIdWithError(SearchId(now.toString), "There must be something wrong with your summary table for the raw file. MsViz does not accept empty values.")
        Failure(e)
      }
    })

    val allMzMlFound = checkMzMlFilesExist(mzMlFiles.get, searchIds.get)

    // if not all mzML files are available we throw an exception
    if (allMzMlFound) {
      // check if searchIds are already taken
      val searchIdAlreadyExists = checkSearchIdsExists(searchIds.get)
      searchIdAlreadyExists.flatMap(alreadyExists => {
        if (alreadyExists._1) {
          val searchIdsString = searchIds.get.map(_.value).reduceLeft(_ + "," + _)
          throw new ImporterException(s"Some of the SearchIds [$searchIdsString] already exist.")
        }
        else insertAllData(searchIds.get, path, mzMlFiles.get, intensityThreshold)
      })
    }
    else {
      throw new ImporterException(s"There are mzMl files missing in the given zip file: [$path]")
    }

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
    val filesFound = mzMlFiles.zip(searchIds).map({ case (mzMlFile, searchId) =>
      if (!Files.exists(mzMlFile.toPath)) {
        searchInfoService.createSearchIdWithError(searchId, s"Error while parsing [${searchId.value}]. Could not find mzML file [${mzMlFile.getName}]")
        false
      } else true
    })

    filesFound.reduceLeft(_ & _)
  }


  /**
   * Check if any of the given SearchIds already exists.
   * In case it already exists we create an error SearchId for the given SearchId
   * @param searchIds
   * @return
   */
  def checkSearchIdsExists(searchIds: Seq[SearchId]): Future[(Boolean, List[SearchId])] = {
    // assert that searchIds are not already inserted
    val searchIdCheck: Seq[Future[(Boolean, SearchId)]] = searchIds.map({ id =>
      searchInfoService.isSearchIdExist(id).map({ alreadyTaken =>
        if (alreadyTaken) searchInfoService.createSearchIdWithError(id, s"SearchId [${id.value}] already exists. Please delete SearchIds with this name before reloading")
        (alreadyTaken, id)
      })
    })

    // check if all searchIds are ok
    val failedSearchIds: List[SearchId] = List()

    val searchIdAlreadyExists: Future[(Boolean, List[SearchId])] = Future.sequence(searchIdCheck).map({
      found => found.foldLeft((false, failedSearchIds))({ (a, b) =>
        ((a._1 | b._1), if (b._1) b._2 :: a._2 else a._2)
      })
    })

    searchIdAlreadyExists
  }

  /**
   * insert all data to the database
   *
   * @param searchIds
   * @param path
   * @param mzMlFiles
   * @param intensityThreshold
   * @return
   */
  def insertAllData(searchIds: List[SearchId],
                    path: String,
                    mzMlFiles: List[File],
                    intensityThreshold: Double): Future[Seq[SearchId]] = {

    // the callback to update the searchId status
    def updateStatus(searchId: SearchId, code: String, message: String) = {
      val status = new SubmissionStatus(code = code, message = message)
      searchInfoService.updateStatus(searchId, status)
    }

    val insertedIds = for {
      nrMatch <- insertMatchData(path, searchIds, Some(updateStatus))
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
        throw new ImporterException("Error while loading experimental data. " + e.getMessage, e)
      }
    })

  }

  /**
   * insert psm, proteinInfo and searchInfo data from a list of mzId files
   *
   * @param innerPath
   * @param searchIds
   * @param updateStatusCallback
   * @return
   */
  def insertMatchData(innerPath: String,
                      searchIds: List[SearchId],
                      updateStatusCallback: Option[(SearchId, String, String) => Future[Boolean]] = None): Future[Int] = {
    //Load maxQuant results
    val maxqResults = LoaderMaxQuant.parse(innerPath.toString + "/txt/", Some("MXQ_"))

    val numberEntries: Seq[Future[Int]] = maxqResults.map({ psmAndProteinList: (Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo) =>
      for {
      // and only last the other data
        psmNr <- matchService.insert(psmAndProteinList._1)
        proteinNumber <- protMatchService.insert(psmAndProteinList._2)
        searchOk <- searchInfoService.insert(psmAndProteinList._3)

      } yield {
        if (updateStatusCallback.isDefined) {
          searchIds.foreach(searchId => updateStatusCallback.get.apply(searchId, "loading", "waiting to load experimental data"))
        }
        psmNr + proteinNumber + (if (searchOk) 1 else 0)
      }
    })

    Future.sequence(numberEntries).map(_.sum)
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
    mzMlFiles.foldLeft(Future {
      0
    })((futureA, b) =>
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

    val itMs1Ms2 = Try(LoaderMzML().parse(mzMlFile, RunId(id.value))).recoverWith({
      case NonFatal(e) => {
        val errorMessage = s"Error while parsing MzML file. There is something wrong with MzML file [${mzMlFile.getName}]"
        searchInfoService.createSearchIdWithError(id, errorMessage)
        Failure(new ImporterException(errorMessage, e))
      }
    })

    val itMs1Ms2parts = itMs1Ms2.get.partition(_.isLeft)

    val itMs1: Iterator[ExpMs1Spectrum] = itMs1Ms2parts._1.map(_.left.get)
    val itMs2: Iterator[ExpMSnSpectrum] = itMs1Ms2parts._2.map(_.right.get)

    updateStatusCallback.get.apply(id, "loading", "loading ms1 data")

    val insertMs1: Future[Boolean] = ms1Service.insertMs1spectra(itMs1, intensityThreshold)

    insertMs1.recover({
      case NonFatal(e) => {
        val errorMessage = s"Error while loading ms1 data."
        searchInfoService.createSearchIdWithError(id, errorMessage)
        throw new ImporterException(errorMessage, e)
      }
    })

    insertMs1.flatMap({ ms1Inserted =>
      if (updateStatusCallback.isDefined) {
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
        if (updateStatusCallback.isDefined) {
          updateStatusCallback.get.apply(id, "loading", "finished ms2 load")
        }

        (if (ms1Inserted) 0 else 1) + ms2Inserted
      })
    })

  }

}

  /**
* the companion object
*/

object LoaderMQData extends Controller with MongoController {

val default = new LoaderMQData(db)

def apply() = default

}
