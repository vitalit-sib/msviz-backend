package ch.isbsib.proteomics.mzviz.uploads

import java.io.File

import ch.isbsib.proteomics.mzviz.commons.helpers.{FileFinder, Unzip}
import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMzML
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, ExpMs1Spectrum}
import ch.isbsib.proteomics.mzviz.experimental.services.{ExpMongoDBService, ExpMs1BinMongoDBService}
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMaxQuant
import ch.isbsib.proteomics.mzviz.matches.services.{SearchInfoDBService, ProteinMatchMongoDBService, MatchMongoDBService}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future



/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMQData(val db: DefaultDB) {

  /**
   * load a zip file containing mzML and txt folder with MQ results
   *
   * @param zipPath
   * @return
   */
  def loadZip(zipPath: String, intensityThreshold:Double): Future[Int] = {
    // unzip the file
    val unzipPath = FileFinder.getHighestDir(Unzip.unzip(new File(zipPath)))


    // get the list of files
    val fileList = FileFinder.getListOfFiles(unzipPath)

    //parse txt/summary to obtain check if we have all expected files
    val summaryFile = unzipPath + "/txt/summary.txt"
    val summaryHash = LoaderMaxQuant.parseMaxquantSummaryTableRawSearchId(new File(summaryFile))

    //Check if all mzML files are available
    summaryHash.keys.foreach {
        key =>
          if (!fileList.contains(new File(unzipPath + "/" + key + ".mzML"))) {
            throw new RuntimeException("File" + unzipPath + "/" + key + ".mzML" + "not found")
          }
    }

    //Load mzML files
    val itTotalEntries=summaryHash.keys.map {
        file => {
          //Load ms1 and ms2
          val itMs1Ms2 = LoaderMzML().parse(new File(unzipPath + "/" + file + ".mzML"), RunId(summaryHash.get(file).get)).partition(_.isLeft)
          val itMs1: Iterator[ExpMs1Spectrum] = itMs1Ms2._1.map(_.left.get)
          val itMs2: Iterator[ExpMSnSpectrum] = itMs1Ms2._2.map(_.right.get)
          //Load maxQuant results
          val maxqResults= LoaderMaxQuant.parse(unzipPath.toString + "/txt/",None)

          maxqResults.foreach({ psmAndProteinList =>
            new MatchMongoDBService(db).insert(psmAndProteinList._1)
            new ProteinMatchMongoDBService(db).insert(psmAndProteinList._2)
            new SearchInfoDBService(db).insert(psmAndProteinList._3)
          })

          // calculate number of entries per each ms to check in the test
          for {

          //Load MS1
            ms1 <- new ExpMs1BinMongoDBService(db).insertMs1spectra(itMs1, intensityThreshold)
            //Load MS2
            ms2 <- new ExpMongoDBService(db).insertMs2spectra(itMs2, RunId(summaryHash.get(file).get))
          }yield{
            ms1 + ms2

          }

        }
    }
    Future.sequence(itTotalEntries.toList).map(_.sum)
  }

}

/**
 * the companion object
 */

object LoaderMQData extends Controller with MongoController {

  val default = new LoaderMQData(db)

  def apply() = default

}
