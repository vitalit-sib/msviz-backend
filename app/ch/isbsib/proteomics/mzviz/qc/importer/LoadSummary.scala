package ch.isbsib.proteomics.mzviz.qc.importer
import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.qc._
import ch.isbsib.proteomics.mzviz.qc.models.{QcSummaryEntry}

import scala.io.Source
import scala.reflect.io
import scala.util.{Failure, Success, Try}
import scalaz.std.map

/**
 * Created by qjolliet on 27/07/15.
 */
class LoadSummary (file:String) {


  /**
   * parser a summary.txt make all collums values into a List of the map with header/value.
   ** @param filename  summary.txt to read from
   **/
  val itLines = Source.fromFile(file).getLines()
  //extract the header line to a list*

  def getSummaryEntry:Seq[QcSummaryEntry] = {itLines.toList.drop(1).dropRight(1).map({ s=>
      val data = s.split("\t")
      val rawfile = getInfo(data(0))
      val len= rawfile.length
    QcSummaryEntry(rawfile(len-2),rawfile(len-1),data(14).toInt,data(15).toInt,data(20).toInt,data(28).toInt)
  })}

  def getInfo(rawfile:String) = {
    rawfile.split("_")
  }

}
/**
 * companion object
 */
object LoadSummary {

  def apply(filename: String) = new LoadSummary(filename)


}



