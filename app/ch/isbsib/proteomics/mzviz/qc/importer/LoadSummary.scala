package ch.isbsib.proteomics.mzviz.qc.importer
import java.io.File
import java.text.SimpleDateFormat
import java.util

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.qc._
import ch.isbsib.proteomics.mzviz.qc.models.{QcSummaryEntry, RawfileInfomation}




import scala.io.Source
import scala.reflect.io
import scala.util.{Failure, Success, Try}
import scalaz.std.map
import scalaz.syntax.std.option

/**
 * Created by qjolliet on 27/07/15.
 */
class LoadSummary (file:File) {


  /**
   * parser a summary.txt make all collums values into a List of the map with header/value.
   ** @param filename  summary.txt to read from
   **/
  val itLines = Source.fromFile(file).getLines()

  //extract the header line to a list*
  val header = itLines.take(1).next.split("\t").toList

  //extract the Summary lines except total line
  val mSummary = itLines.toList.dropRight(1).map (s => (header zip s.split("\t")).toMap)
  /***
  def getSummaryEntry:Seq[QcSummaryEntry] = itLines.toList.drop(1).dropRight(1).map({ s=>
      val data = s.split("\t")
      val rawfile = getInfo(data(0))
      val protein = rawfile(0)
      val quantity = rawfile(1)
      val machineName = rawfile(2)
      val columnType = rawfile(3) + "_" + rawfile(4)

      val Date = if (isAllDigits(rawfile(5)))  rawfile(5)
                 else throw new SummaryParsingException(s"It's a value not valide for date:\n$rawfile(5)")

      val Index = rawfile(6)

      val rawfileInfomation=RawfileInfomation(protein,quantity,machineName,columnType,Date,Index)

      QcSummaryEntry(rawfileInfomation,data(14).toInt,data(15).toInt,data(20).toInt,data(28).toInt)
  })
  ***/
  def getSummaryEntry:Seq[QcSummaryEntry] = mSummary.map{
      m => {
        val rawfile = getInfo(m("Raw file"))
        val proteinName = ProteinName(rawfile(0))
        val proteinQuantity = ProteinQuantity(rawfile(1))
        val machineName = MachineName(rawfile(2))
        val columnType = ColumnType(rawfile(3) + "_" + rawfile(4))
        //var Date = if (isAllDigits(rawfile(5)))  rawfile(5)
                   //else throw new SummaryParsingException(s"It's not a valide date:\n$rawfile(5)")
        val qcDate= QcDate(parserDate(rawfile(5)))
        val qcIndex = QcIndex(rawfile(6))
        val rawfileInfo=RawfileInfomation(proteinName,proteinQuantity,machineName,columnType,qcDate,qcIndex)
        QcSummaryEntry(rawfileInfo,m("MS").toInt,m("MS/MS").toInt,m("MS/MS Identified").toInt,m("Peptide Sequences Identified").toInt)
      }
  }


  def getInfo(rawfile:String) = {
    rawfile.split("_")
  }
/***
  def isAllDigits(x: String) = x forall Character.isDigit
  //val reDate="""(\d{6})""".r
  def parserDate(x:String):String = x match{
        case reDate(x) => x
        case _ => throw new SummaryParsingException(s"It's not a valide date:\n$x")
    }
  ***/

def parserDate(x:String): util.Date = try
  {
    val formatter = new SimpleDateFormat("yyMMdd")
    formatter.parse(x)
  } catch {
    case e:Exception =>throw new SummaryParsingException(s"It's not a valide date:\n$x")
}




}

/**
 * companion object
 */
object LoadSummary {

  def apply(filename: String) = new LoadSummary(new File(filename))
  def apply(file: File) = new LoadSummary(file)

}



