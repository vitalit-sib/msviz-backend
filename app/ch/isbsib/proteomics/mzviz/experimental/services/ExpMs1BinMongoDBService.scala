package ch.isbsib.proteomics.mzviz.experimental.services

import ch.isbsib.proteomics.mzviz.commons.{Intensity, Moz, RetentionTime}
import ch.isbsib.proteomics.mzviz.commons.services.{MongoInsertException, MongoNotFoundException, MongoDBService}
import ch.isbsib.proteomics.mzviz.experimental.services.JsonExpFormats._
import ch.isbsib.proteomics.mzviz.experimental.{RunIdAndMozBin, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models._
import play.api.libs.json._
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.{IndexType, Index}
import play.api.libs.concurrent.Execution.Implicits._
import reactivemongo.bson.{BSONString, BSONArray, BSONDocument}
import reactivemongo.core.commands.LastError

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.{Success, Failure}

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ExpMs1BinMongoDBService (val db: DefaultDB) extends MongoDBService {
  val collectionName = "ms1Bins"
  val mainKeyName = "ref"

  new Index(
    Seq("ref" -> IndexType.Ascending),
    name = Some("ref")
  )


  //we put a implicit JSON serilizer here, the JSON Mongo format is difference from the JSON web format
  // peak array are serialized for sake of speed
  implicit val formatMs1EntryList = new Format[Ms1EntryList] {

    override def reads(json: JsValue): JsResult[Ms1EntryList] = {
      val ref = (JsPath \ "ref").read[RunIdAndMozBin].reads(json).get

      //re-assemble the peaks
      val mozs: List[Double] = decode64(json.validate[String]((JsPath \ "peaks" \ "mozs").read[String]).get)
      val intensities: List[Double] = decode64(json.validate[String]((JsPath \ "peaks" \ "intensities").read[String]).get)
      val retentionTimes: List[Double] = decode64(json.validate[String]((JsPath \ "peaks" \ "retentionTimes").read[String]).get)

      val peaks: Seq[Ms1Entry] =
        for {
          ((m: Double, i: Double), r: Double) <- mozs.zip(intensities).zip(retentionTimes)
        } yield {
          Ms1Entry(moz = Moz(m), intensity = Intensity(i), rt = RetentionTime(r))
        }

      JsSuccess(
        Ms1EntryList(ref = ref, ms1EntryList = peaks)
      )

    }

    def writes(o: Ms1EntryList) = {

      Json.obj(
        "ref" -> o.ref.value,
        "peaks" -> Json.obj(
          "mozs" -> encode64[Double](o.ms1EntryList.map(_.moz.value).toList),
          "intensities" -> encode64[Double](o.ms1EntryList.map(_.intensity.value).toList),
          "retentionTimes" -> encode64[Double](o.ms1EntryList.map(_.rt.value).toList)
        )
      )
    }
  }

  /**
   * insert MS1 peaks into the temporary collection "ms1Peaks"
   *
   * @param intensityThreshold all peaks below this threshold will be ignored
   * @param ms1SpList list of ExpMs1Spectum's
   * @return gives back the lowest (left) and highest (right) Moz value and number of entries inserted into the db
   */
  def insertMS1peaks(ms1SpList: Iterator[ExpMs1Spectrum], intensityThreshold:Double): (Double, Double, Future[Int]) ={

    var lowestMoz:Double = 9999999
    var highestMoz:Double = 0
    var inserts: ListBuffer[Future[Int]] = ListBuffer()

    // insert all MS1 spectra and keep lowest and highest moz (we need that later for the bins)
    while(ms1SpList.hasNext){
      val ms1 = ms1SpList.next()
      val res = ExpMs1MongoDBService().insertMS1(ms1, intensityThreshold)

      if(res._2 < lowestMoz) lowestMoz = res._2
      if(res._3 > lowestMoz) highestMoz = res._3

      inserts += res._1
    }

    val totalInserts:Future[Int] = Future.sequence(inserts.toList).map(_.sum)

    (lowestMoz, highestMoz, totalInserts)
  }

  /**
   * Insert an ms1EntryList
   *
   * @param ms1EntryList
   * @return
   */
  def insertMs1EntryList(ms1EntryList:Ms1EntryList): Future[Boolean] = {
    val future = collection.insert(ms1EntryList)

    val answer:Future[Boolean] = future.map({
      case e: LastError if e.inError => throw MongoInsertException(e.errMsg.get)
      case _ => true
    })

    answer
  }


  def findMs1EntryWithMozTol(runId: RunId, moz: Moz, tolerance:Double): Future[Seq[Ms1Entry]] = {

    // the bin we're looking at
    val mozBin = moz.value.toInt

    // the first part of the ref
    val runRef = runId.value + "_"

    println("ref: " + runRef)

    // if the tolerance is at the border of a bin, we have to take the neighboring bin one as well
    val secondPart = if((mozBin - tolerance).toInt < mozBin){
      RunIdAndMozBin(runRef + (mozBin-1).toString) :: Nil
    } else if((mozBin - tolerance).toInt < mozBin){
      RunIdAndMozBin(runRef + (mozBin-1).toString) :: Nil
    }else{
      Nil
    }

    val refSet:Set[RunIdAndMozBin] = (RunIdAndMozBin(runRef + mozBin) :: secondPart).toSet

    refSet.foreach(a => println(a.value))

    // get list of Ms1Entries
    val res = findMs1EntryList(refSet)
    res.map(a => println(a.size))

    val futureEntryList: Future[List[Ms1Entry]] = res.map(oneEntry => oneEntry.flatMap(_.ms1EntryList))

    futureEntryList.map(a => println(a.size))

    // filter entries on given moz and tolerance
    futureEntryList.map(entryList => entryList.filter(a => a.moz.value <= moz.value+tolerance && a.moz.value >= moz.value-tolerance))
  }

  /**
   * Get the Ms1EntryList for a given ref
   *
   * @param refs
   * @return
   */
  def findMs1EntryList(refs: Set[RunIdAndMozBin]): Future[List[Ms1EntryList]] = {

    val query = refs.toList match {
      case x :: Nil => Json.obj("ref" -> x.value)
      case xs => Json.obj("ref" -> Json.obj("$in" -> xs.map(id => id.value)))
    }

    collection.find(query).cursor[Ms1EntryList].collect[List]()
  }

  /**
   * Loop through all the bins (from lowest moz to highest moz) and get all entries from the "ms1Peaks" collection.
   * Then create a Ms1EntryList and insert it to the collection "ms1Bins". The name of the bin consists of:
   * RunId + _ + bin (e.g. F000345_400).
   *
   * @param runId
   * @param lowestMoz
   * @param highestMoz
   * @return
   */
  def createMS1bins(runId: RunId, lowestMoz:Double, highestMoz: Double): Future[Int] = {

    // loop through all bins
    for(bin <- lowestMoz.toInt to highestMoz.toInt){
      val futureEntries:Future[List[Ms1EntryWithRef]] = ExpMs1MongoDBService().findMs1ByRunID_MozBorders(runId, Moz(bin.toDouble), Moz((bin+1).toDouble))

      val ref = RunIdAndMozBin(runId + "_" + bin.toString)

      futureEntries.map({ entriesWithRef =>
        val entries = entriesWithRef.map(a => Ms1Entry(a.rt, a.intensity, a.moz))
        val ms1EntryList =  Ms1EntryList(ref, entries)
      })

    }

    Future{1}
  }


}
