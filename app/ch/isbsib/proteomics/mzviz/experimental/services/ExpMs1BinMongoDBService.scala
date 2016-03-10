package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.services.{MongoInsertException, MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.{RunIdAndMozBin, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models._
import play.api.libs.json.{JsObject, JsNumber, JsArray, Json}
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import reactivemongo.core.commands.LastError
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import ch.isbsib.proteomics.mzviz.commons.{Intensity, Moz, RetentionTime}
import scala.collection.mutable.ListBuffer
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1BinMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "ms1Bin"
  val mainKeyName = "ref"

  new Index(
    Seq("ref" -> IndexType.Ascending),
    name = Some("ref")
  )


  /**
   * insert a list of MS1 spectra into a temporary collection "ms1Tmp"
   *
   * @param intensityThreshold all peaks below this threshold will be ignored
   * @param ms1Iterator ExpMs1Spectum
   * @return gives back the number of entries inserted into the db
   */
  def insertMs1spectra(ms1Iterator: Iterator[ExpMs1Spectrum], intensityThreshold:Double): Future[Int] = {

    // number of spectra which are parsed before inserting
    val bufferSize = 20

    // split the iterator into slices
    val slidingIt = ms1Iterator.sliding(bufferSize, bufferSize)

    var resList:ListBuffer[Future[Int]] = ListBuffer()

    // loop through all slices
    while(slidingIt.hasNext){
        val initMap:Map[String,(Seq[Moz], Seq[Intensity], Seq[RetentionTime])] = Map()

        val someList = slidingIt.next()

        val resMap = someList.par.foldLeft(initMap)((a,b) => mergeBinMaps(a, parseMs1spectrum(b, intensityThreshold)))
        resList += insertMs1Bin(resMap)
    }

    Future.sequence(resList.toList).map(_.sum)

  }

  /**
   * insert a MS1 spectrum into a temporary collection "ms1Tmp"
   *
   * @param intensityThreshold all peaks below this threshold will be ignored
   * @param ms1 ExpMs1Spectum
   * @return gives back the number of entries inserted into the db
   */
  def parseMs1spectrum(ms1: ExpMs1Spectrum, intensityThreshold:Double): Map[String, (Seq[Moz], Seq[Intensity], Seq[RetentionTime])] = {

    val filteredPeaks = ms1.peaks.filter(_.intensity.value >= intensityThreshold)

    val peakMap:List[(String,(Seq[Moz], Seq[Intensity], Seq[RetentionTime]))] = filteredPeaks.map({peak =>

      // prepare the reference (of type SearchId_Moz, e.g. F003965_487)
      val ref = ms1.spId.runId.value + "_" + peak.moz.value.toInt.toString

      (ref,(Seq(peak.moz), Seq(peak.intensity), Seq(ms1.retentionTime)))
    })

    val initMap:Map[String,(Seq[Moz], Seq[Intensity], Seq[RetentionTime])] = Map()

    // transform list to concatenated maps
    peakMap.foldLeft(initMap)({ (a,b) =>
      // add values to an existing key
      if(a.contains(b._1)){
        val o = a(b._1)
        a ++ Map(b._1 -> (o._1 ++ b._2._1, (o._2 ++ b._2._2), (o._3 ++ b._2._3)))
        // or add a new entry
      }else{
        a ++ Map(b._1 -> b._2)
      }
    })

  }


  /**
   * Merge 2 maps together. List of moz, int and rt are merged together under corresponding keys.
   *
   * @param map1
   * @param map2
   * @return
   */
  def mergeBinMaps(map1: Map[String, (Seq[Moz], Seq[Intensity], Seq[RetentionTime])],
                   map2: Map[String, (Seq[Moz], Seq[Intensity], Seq[RetentionTime])]): Map[String, (Seq[Moz], Seq[Intensity], Seq[RetentionTime])] = {

    map1.foldLeft(map2)({ (a,b) =>
      // add values to an existing key
      if(a.contains(b._1)){
        val o = a(b._1)
        a ++ Map(b._1 -> (o._1 ++ b._2._1, (o._2 ++ b._2._2), (o._3 ++ b._2._3)))
        // or add a new entry
      }else{
        a ++ Map(b._1 -> b._2)
      }
    })

  }


  /**
   * Insert a given Map into the database
   *
   * @param ms1Bin
   * @return
   */
  def insertMs1Bin(ms1Bin: Map[String, (Seq[Moz], Seq[Intensity], Seq[RetentionTime])]): Future[Int] = {

    Future.sequence(ms1Bin.map({entry =>
      val selector = Json.obj("ref" -> entry._1)

      val newEntry = Json.obj("$push" -> Json.obj(
        "mozList" -> Json.obj("$each" -> JsArray(entry._2._1.map(a => JsNumber(a.value)))),
        "intList" -> Json.obj("$each" -> JsArray(entry._2._2.map(a => JsNumber(a.value)))),
        "rtList" -> Json.obj("$each" -> JsArray(entry._2._3.map(a => JsNumber(a.value))))
        )
      )

      // create or update
      val answer = collection.update(selector, newEntry, upsert = true)

      answer.map({
        case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
        case _ => 1
      })

    }).toList).map(_.sum)

  }


  /**
   * load the correct bin(s) and filter the entries by moz and tolerance
   *
   * @param runId
   * @param moz
   * @param tolerance
   * @return
   */
  def findMs1EntryWithMozTol(runId: RunId, moz: Moz, tolerance:Double): Future[List[Ms1Entry]] = {

    // the bin we're looking at
    val mozBin = moz.value.toInt

    // the first part of the ref
    val runRef = runId.value + "_"

    // if the tolerance is at the border of a bin, we have to take the neighboring bin one as well
    val secondPart = if((mozBin - tolerance).toInt < mozBin){
      RunIdAndMozBin(runRef + (mozBin-1).toString) :: Nil
    } else if((mozBin - tolerance).toInt < mozBin){
      RunIdAndMozBin(runRef + (mozBin-1).toString) :: Nil
    }else{
      Nil
    }

    val refSet:Set[RunIdAndMozBin] = (RunIdAndMozBin(runRef + mozBin) :: secondPart).toSet

    // get list of Ms1Entries
    val res = findMs1EntryList(refSet)
    val futureEntryList: Future[List[Ms1Entry]] = res.map(binList => binList.flatMap(oneBin => oneBin.rtList.zip(oneBin.intList.zip(oneBin.mozList)).map(a => Ms1Entry(a._1, a._2._1, a._2._2))))

    // filter entries on given moz and tolerance
    val filteredList = futureEntryList.map(entryList => entryList.filter(a => a.moz.value <= moz.value+tolerance && a.moz.value >= moz.value-tolerance))

    filteredList
  }

  /**
   * Get the Ms1EntryList for a given ref
   *
   * @param refs
   * @return
   */
  def findMs1EntryList(refs: Set[RunIdAndMozBin]): Future[List[Ms1Bin]] = {

    val query = refs.toList match {
      case x :: Nil => Json.obj("ref" -> x.value)
      case xs => Json.obj("ref" -> Json.obj("$in" -> xs.map(id => id.value)))
    }

    collection.find(query).cursor[Ms1Bin].collect[List]()
  }


  /**
   * Delete all bins containing the given RunIds
   *
   * @param runIds
   * @return
   */
  def deleteAllByRunIds(runIds: Set[RunId]): Future[Int] = {

    Future.sequence(runIds.map({ runId =>

      // create a regex which looks for all occurences of the given RunId
      val regexString = "^" + runId.value + "_"
      val query = Json.obj("ref" -> Json.obj("$regex" -> regexString))

      collection.remove(query).map {
        case e: LastError if e.inError => throw MongoNotFoundException(e.errMsg.get)
        case _ => 1
      }

    })).map(_.sum)
  }


  /**
   * sort the retention times and add 0 intensity points in large gaps
   *
   * @param futureMs1List
   * @param rtTolerance
   * @return
   */
  def extract2Lists(futureMs1List:Future[List[Ms1Entry]], rtTolerance: Double): Future[JsObject] = {

    futureMs1List.map({ ms1List =>

      if (ms1List.length > 0) {
        // group by retentionTimes
        val rtGroups = ms1List.groupBy(_.rt.value)

        // sum the groups => Map(rt, max(intensities))
        val summedMap = rtGroups.map({ case (k, v) => (k, v.map(_.intensity.value).max) })

        // sort by rt separate the lists and make a Json-object
        val sortedSums = summedMap.toSeq.sortBy(_._1)

        // function to add 0 values between peaks which are not close enough
        def checkAndAdd(b: List[(Double, Double)], a: (Double, Double), maxDiff: Double, f: (Double, Double, Double) => List[(Double, Double)]): List[(Double, Double)] = {
          if (b.last._1 + rtTolerance < a._1) {
            b ++ f(b.last._1, a._1, maxDiff) :+ a
          } else b :+ a
        }

        // helper function to add 0 values
        def addZeroValues(val1: Double, val2: Double, maxDiff: Double): List[(Double, Double)] = {
          if (val1 + maxDiff > val2 - maxDiff) {
            List(Tuple2((val1 + val2) / 2, 0.0))
          } else {
            List(Tuple2(val1 + maxDiff, 0.0), Tuple2(val2 - maxDiff, 0.0))
          }
        }

        val addedMissingRts = sortedSums.drop(1).foldLeft(List(sortedSums(0)))((b, a) => checkAndAdd(b, a, rtTolerance, addZeroValues))

        val aux = addedMissingRts.unzip
        Json.obj("rt" -> aux._1, "intensities" -> aux._2)
      } else {
        val emptyList: List[Ms1EntryWithRef] = List()
        Json.obj("rt" -> emptyList, "intensities" -> emptyList)
      }
    })
  }

}


/**
 * the companion object
 */
object ExpMs1BinMongoDBService extends Controller with MongoController {
  val default = new ExpMs1BinMongoDBService(db)

  /**
   * get the default db/collection
   * @return
   */
  def apply() = default

}