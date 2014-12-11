package ch.isbsib.proteomics.mzviz.experimental.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.models._
import org.expasy.mzjava.core.ms.spectrum
import play.api.Logger

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Load an MGF file into an MSRun
 * @author Alexandre Masselot
 */
object LoaderMGF {
  val reEqual = """(\w.*?)=(.*)""".r
  val rePeak = """^(\d\S+)\s*(\d\S+)?\s*$""".r

  /**
   * extracts the xxx=yyy as a map
   * @param text BEGIN/END IONS block
   * @return
   */
  def text2map(text: String): Map[String, String] = {
    val lRet: Seq[Tuple2[String, String]] =
      text.split("\n").toSeq
        .filter(l => reEqual.findFirstIn(l).isDefined)
        .map(l => l match {
        case reEqual(k, v) => (k, v.trim)
        case _ => throw new IllegalArgumentException(s"how can [$l] pass findFirst and not being matched")
      })
    Map(lRet: _*)
  }


  /**
   * extract all peaks from lines with one or two double
   * if only one is present (as it can be the case in some precursor, we'll assume 0 intensity
   * @param l  a string
   * @return
   */
  def textLine2MozIntensity(l: Option[String]): Try[Tuple2[Moz, Intensity]] = {
    l match {
      case None => Failure(new IllegalArgumentException(s"peak cannot be extract from None string"))
      case Some(rePeak(m, null)) => Success(Tuple2(Moz(m.toDouble), Intensity(0)))
      case Some(rePeak(m, i)) => Success(Tuple2(Moz(m.toDouble), Intensity(i.toDouble)))
      case _ => Failure(new IllegalArgumentException(s"peak cannot be extract from [$l]"))
    }
  }

  /**
   * from an MGF MSn text block, extract all peaks
   * @param text BEGIN/END IONS text block
   * @return
   */
  def text2peaks(text: String): Try[Seq[ExpPeakMSn]] = {
    val lTry = text.split("\n").toSeq
      .filter(l => rePeak.findFirstIn(l).isDefined)
      .zipWithIndex
      .map({

      case (l, iRank) => val t = textLine2MozIntensity(Some(l))
        t.map({ case (m, i) => ExpPeakMSn(m, i, IntensityRank(iRank), MSLevel(2))})
    })
    lTry.find(_.isFailure) match {
      case None => Success(lTry.map(_.get))
      case Some(e: Exception) => Failure(e)
    }
  }

  val reRTTitleWiff = """.*Elution:\s+([0-9\.]+)\s+min.*""".r
  val reRTTitleWiffInterval = """.*Elution:\s+([0-9\.]+)\s+to\s+([0-9\.]+)\s+min.*""".r

  /**
   * try to read retention time from RTINSECONDS, RTINSECONDS[0] fields or parsed out from TITLE line
   * @param args
   * @return
   */
  def args2RT(args: Map[String, String]): Try[RetentionTime] = Try {
    val rt = args.get("RTINSECONDS")
      .orElse(args.get("RTINSECONDS[0]"))
      .orElse(
        args.get("TITLE") match {
          case Some(reRTTitleWiff(rt)) => Some((rt.toDouble * 60).toString)
          case Some(reRTTitleWiffInterval(rtFrom, rtTo)) => Some((30*(rtFrom.toDouble +rtTo.toDouble)).toString)
          case None => {
            throw new UnsupportedOperationException(s"cannot parse retention time from $args, neither from RTINSECONDS nor TITLE")
          }
          case _ => {
            throw new UnsupportedOperationException(s"""cannot parse retention time from TITLE: ${args.get("TITLE")}""")
          }
        }
      ).get

    RetentionTime(rt.toDouble)
  }


  /**
   * convert an MGF BEING/END IONS block into the precursor peak
   * @param text MGF block
   * @return
   */
  def text2Precursor(text: String, idRun: Option[IdRun]): Try[RefSpectrum] = {
    val reTitleScan = """.*\.(\d+)\.\d$""".r
    val args = text2map(text)
    for {
      (moz, intens) <- textLine2MozIntensity(args.get("PEPMASS"))
    } yield {
      val z = args("CHARGE").replace("+", "").toInt
      val rt = args2RT(args).getOrElse(RetentionTime(-1))
      val title = args.getOrElse("TITLE", "")

      val scanNumber = args.getOrElse("SCANNUMBER",
        title match {
          case reTitleScan(s) => s
          case _ => "-1"
        }
      )

      RefSpectrum(
        scanNumber = ScanNumber(scanNumber.toInt),
        precursor = ExpPeakPrecursor(moz, intens, rt, Charge(z)),
        title = title,
        idRun = idRun
      )
    }
  }


  /**
   * convert an MGF BEING/END IONS block into an MSs spectrum
   * @param text MGF block
   * @return
   */
  def text2MSnSpectrum(text: String, idRun: Option[IdRun]): Try[ExpMSnSpectrum] = {
    for {
      ref <- text2Precursor(text, idRun)
      peaks <- text2peaks(text)
    } yield {
      ExpMSnSpectrum(ref = ref, peaks)
    }
  }


  /**
   * Loads an MGF file. peak order is taken out from the MGF file order as this makes sense in our examples
   *
   * @param filename .mgf file
   * @param idRun default is idRun is taken out from file basename
   * @return
   */
  def load(filename: String, idRun: Option[String] = None): MSRun = {
    val actualIdRun = IdRun(idRun.getOrElse(new File(filename).getName.replace(".mgf", "")))

    val lPeaks: Seq[ExpMSnSpectrum] = new IonsIterator(filename)
      .map(t => text2MSnSpectrum(t, Some(actualIdRun)))
      .filter({
      case (Failure(e)) => println(e.getMessage)
        false
      case (Success(t)) => true
    })
      .map(_.get)
      .toSeq
    new MSRun(actualIdRun, lPeaks)
  }

  /**
   * produces an iterator over BEGIN/END IONS
   * @param filename mgf file to read from
   */
  class IonsIterator(filename: String) extends Iterator[String] {
    val itLines = Source.fromFile(filename).getLines()

    /**
    Gets the next chunk, if any
      */
    def readNextChunk: Option[String] = {
      if (!itLines.hasNext) {
        None
      } else {
        val s = itLines.takeWhile(!_.toUpperCase.startsWith("END IONS")).toList :+ "END IONS"
        if (s.filterNot(_.trim == "").size == 0)
          None
        else {
          Some(s.mkString("\n"))
        }
      }
    }

    var pHasNext: Option[Boolean] = None
    var pNextChunk: Option[String] = None


    override def hasNext: Boolean = pHasNext match {
      case Some(b) => b
      case None =>
        pNextChunk = readNextChunk
        pHasNext = Some(pNextChunk.isDefined)
        pNextChunk.isDefined
    }

    override def next(): String = if (hasNext) {
      val ret = pNextChunk.get
      pHasNext = None
      ret
    } else
      throw new IllegalArgumentException("Cannot call next when not having next element")

  }


}


