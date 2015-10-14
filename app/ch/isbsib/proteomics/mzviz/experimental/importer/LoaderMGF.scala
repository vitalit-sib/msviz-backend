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
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
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
        .map({
        case reEqual(k, v) => (k, v.trim)
        case l => throw new MGFParsingException(s"how can a line pass findFirst and not being matched:\n$l")
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
      case None => Failure(new MGFParsingException(s"peak cannot be extracted from None string"))
      case Some(rePeak(m, null)) => Success(Tuple2(Moz(m.toDouble), Intensity(0)))
      case Some(rePeak(m, i)) => Success(Tuple2(Moz(m.toDouble), Intensity(i.toDouble)))
      case _ => Failure(new IllegalArgumentException(s"peak cannot be extract from [$l]"))
    }
  }

  /**
   * from an MGF MSn text block, extract all peaks
   * We assume that peaks are sorted by "mascot" intensity rank. This is not universale, but suits our first purpose
   * the returned peaks are sorted by increasing m/z, but the instensityr rank is taken out from the inputMGF (that is current mascort default)
   * Only takes maxPeaks peaks
   * @param text BEGIN/END IONS text block
   * @param maxPeaks the firs x peaks to be taken
   * @return
   */
  def text2peaks(text: String, maxPeaks:Int=300): Try[List[ExpPeakMSn]] = {
    val lTry = text.split("\n").toList
      .filter(l => rePeak.findFirstIn(l).isDefined)
      .take(maxPeaks)
      .zipWithIndex
      .map({

      case (l, iRank) => val t = textLine2MozIntensity(Some(l))
        t.map({ case (m, i) => ExpPeakMSn(m, i, IntensityRank(iRank), MSLevel(2))})
    })
    lTry.find(x => x.isFailure) match {
      case None => Success(lTry.map(_.get).sortBy(_.moz.value))
      case Some(Failure(e)) => Failure(e)
    }
  }

  val reRTTitleWiff = """.*Elution:\s+([0-9\.]+)\s+min.*""".r
  val reRTTitleWiffInterval = """.*Elution:\s+([0-9\.]+)\s+to\s+([0-9\.]+)\s+min.*""".r

  /**
   * try to read retention time from RTINSECONDS, RTINSECONDS[0] fields or parsed out from TITLE line
   * @param args the *=* map
   * @return
   */
  def args2RT(args: Map[String, String]): Try[RetentionTime] = Try {
    val ret = args.get("RTINSECONDS")
      .orElse(args.get("RTINSECONDS[0]"))
      .orElse(
        args.get("TITLE") match {
          case Some(reRTTitleWiff(rt)) => Some((rt.toDouble * 60).toString)
          case Some(reRTTitleWiffInterval(rtFrom, rtTo)) => Some((30 * (rtFrom.toDouble + rtTo.toDouble)).toString)
          case None =>
            throw new UnsupportedOperationException(s"cannot parse retention time from $args, neither from RTINSECONDS nor TITLE")
          case _ =>
            throw new UnsupportedOperationException( s"""cannot parse retention time from TITLE: ${args.get("TITLE")}""")
        }
      ).get

    RetentionTime(ret.toDouble)
  }


  /**
   * convert an MGF BEING/END IONS block into the precursor peak
   * @param text MGF block
   * @return
   */
  def text2Precursor(text: String, runId: RunId): Try[SpectrumRef] = {
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

      SpectrumRef(
        scanNumber = ScanNumber(scanNumber.toInt),
        precursor = ExpPeakPrecursor(moz, intens, rt, Charge(z)),
        title = title,
        SpectrumId(id = SpectrumUniqueId(title), runId = runId)
      )
    }
  }


  /**
   * convert an MGF BEING/END IONS block into an MSs spectrum
   * @param text MGF block
   * @return
   */
  def text2MSnSpectrum(text: String, runId: RunId): Try[ExpMSnSpectrum] = {
    for {
      ref <- text2Precursor(text, runId)
      peaks <- text2peaks(text)
    } yield {
      ExpMSnSpectrum(ref = ref, peaks)
    }
  }


  /**
   * Loads an MGF file. peak order is taken out from the MGF file order as this makes sense in our examples
   *
   * @param filename .mgf file
   * @param runId a runId
   * @return
   */
  def load(filename: String, runId: RunId): Try[Iterator[ExpMSnSpectrum]] = load(new File(filename), runId)

  /**
   * Loads an MGF file. peak order is taken out from the MGF file order as this makes sense in our examples
   *
   * @param file .mgf file
   * @param runId the runId under which to register the run
   * @return
   */
  def load(file: File, runId: RunId): Try[Iterator[ExpMSnSpectrum]] = Try {
    val lPeaks: Iterator[ExpMSnSpectrum] = expMSnSpectrumIterator(file, runId)
     // .toSeq
    //new MSRun(runId, lPeaks)
    lPeaks
  }

  /**
   * Instead of loading a full run, just starts an ExpMsSpectrum Iterator
   * @param file
   * @param runId
   * @return
   */
  def expMSnSpectrumIterator(file: File, runId: RunId): Iterator[ExpMSnSpectrum] ={
    new IonsIterator(file)
      .map(t => text2MSnSpectrum(t, runId))
      .filter({
      case (Failure(e)) => throw e
      case (Success(t)) => true
    }).map(_.get)
  }

  /**
   * produces an iterator over BEGIN/END IONS
   * @param file mgf file to read from
   */
  class IonsIterator(file: File) extends Iterator[String] {
    val itLines = Source.fromFile(file).getLines()

    /**
    Gets the next chunk, if any
      */
    def readNextChunk: Option[String] = {
      if (!itLines.hasNext) {
        None
      } else {
        val s = itLines.takeWhile(!_.toUpperCase.startsWith("END IONS")).toList
        if (s.filterNot(_.trim == "").size == 0)
          None
        else {
          Some(s.mkString("\n"+"END IONS\n"))
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


