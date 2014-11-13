package ch.isbsib.proteomics.mzviz.experimental.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, ExpPeak}

import scala.io.Source
import scala.util.matching.Regex

/**
 * Load an MGF file into an MSRun
 * @author Alexandre Masselot
 */
object LoaderMGF {
  /**
   * Loads an MGF file. peak order is taken out from the MGF file order as this makes sense in our examples
   *
   * @param filename .mgf file
   * @param idRun default is idRun is taken out from file basename
   * @return
   */
  def load(filename: String, idRun: Option[String] = None): MSRun = {
    val actualIdRun = IdRun(idRun.getOrElse(new File(filename).getName.replace(".mgf", "")))

    val reEqual = """(\w.*?)=(.*)""".r
    val rePeak = """^(\d\S+)\s+(\d\S+)""".r
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
          case reEqual(k, v) => (k, v)
          case _ => throw new IllegalArgumentException(s"how can [$l] pass findFirst and not being matched")
        })
      Map(lRet: _*)
    }

    def text2peaks(text: String): Seq[ExpPeak] = {
      text.split("\n").toSeq
        .filter(l => rePeak.findFirstIn(l).isDefined)
        .zipWithIndex
        .map({ case (l, iRank) => l match {
        case rePeak(m, i) => ExpPeak(Moz(m.toDouble), Intensity(i.toDouble), IntensityRank(iRank))
        case _ => throw new IllegalArgumentException(s"how can [$l] pass findFirst and not being matched")
      }
      })
    }

    val lPeaks: Seq[ExpMSnSpectrum] = new IonsIterator(filename)
      .map({
      text =>
        null
    }).toSeq
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
        val s = itLines.takeWhile(!_.toUpperCase.startsWith("END IONS"))
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


