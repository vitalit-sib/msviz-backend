package ch.isbsib.proteomics.mzviz.experimental.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.experimental.models.{SpectrumId, ExpMs1Spectrum, ExpPeakMS1}
import ch.isbsib.proteomics.mzviz.commons.{RetentionTime, Intensity, Moz}
import org.apache.commons.codec.binary.Base64
import org.expasy.mzjava.core.io.ms.spectrum.BytesUtils
import scala.xml.XML
import scala.xml.Node

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
/**
 * Load an MzXML file into an MSRun
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object FastLoaderMzXML {

  val base64: Base64 = new Base64

  def parseFile(file: File, runId: RunId):Seq[ExpMs1Spectrum] = {

    val xml = XML.loadFile(file)

    val scans = xml \\ "scan"
    // we just assume that the precision is the same for the whole file
    val firstPeak = (scans(0) \ "peaks")
    val bytePrecision = (firstPeak \ "@precision").text match {
      case "64" => 8
      case "32" => 4
      case s:String => throw new Exception("Illegal precision in MzXML file: " + s)
    }
    // we only parse contentType="m/z-int"
    if((firstPeak \ "@contentType").text != "m/z-int"){
      throw new Exception("Only contentType=m/z-int can be parsed")
    }
    // the compression
    val compression:Boolean = (firstPeak \ "@compressionType").text match {
      case "zlib" => true
      case _ => false
    }

    // keep only ms1 scans
    val ms1Scans = scans.filter(scan => (scan \ "@msLevel").text.toInt == 1)

    ms1Scans.map({ scan =>
      scanToPeaks(scan, bytePrecision, compression, runId)
    })

  }

  /**
   * transform a Scan node to a MS1 spectrum
   *
   * @param scan
   * @param bytePrecision
   * @param compression
   * @param runId
   * @return
   */
  def scanToPeaks(scan:Node, bytePrecision: Int, compression: Boolean, runId: RunId): ExpMs1Spectrum = {

    val scanNumber = (scan \ "@num").text

    // parse peaks
    val expectedPeakCount = (scan \ "@peaksCount").text.toInt
    val peaks = (scan \ "peaks")
    val compressedLen = if(compression) (peaks \ "@compressedLen").text.toInt else 0

    // retention times
    val rtPattern = "PT([\\d|\\.]+)([SM])".r
    val rtPattern(rtValue, rtUnit) = (scan \ "@retentionTime").text
    // transform rt to second unit
    val rt = (if(rtUnit == "M") rtValue * 60 else rtValue).toDouble

    // convert peak string to Byte Array
    val peaksBytes:Array[Byte] = decodePeaks(peaks.text, expectedPeakCount, bytePrecision, compressedLen)
    val expectedByteCount: Int = expectedPeakCount * bytePrecision * 2
    if (peaksBytes.length != expectedByteCount) {
      throw new Exception("peak bytes expected=" + expectedByteCount + ", actual=" + peaksBytes.length + ", actual base64 peaks="  + peaksBytes.length + ": error while parsing <peaks> characters!")
    }

    // convert byte to double
    val dbuf = java.nio.DoubleBuffer.allocate(peaksBytes.length/8)
    dbuf.put(java.nio.ByteBuffer.wrap(peaksBytes).asDoubleBuffer)
    val a = dbuf.array

    // construct peaks
    val extractedPeaks = a.sliding(2,2).map({ peak =>
      ExpPeakMS1(Moz(peak(0)), Intensity(peak(1)))
    }).toList

    // create MS1 spectra
    val spId = new SpectrumId(SpectrumUniqueId(scanNumber), runId)
    ExpMs1Spectrum(spId, RetentionTime(rt), extractedPeaks)
  }


  /**
   * Decode a peak String using the right precions (64 or 32)
   *
   * @param peaks
   * @param expectedPeakCount
   * @param bytePrecision
   * @param compressedLen
   * @return
   */
  def decodePeaks(peaks: String,
                  expectedPeakCount: Int,
                  bytePrecision: Int,
                  compressedLen: Int ) : Array[Byte] = {
    val peaksBytes = base64.decode(peaks)
    if(compressedLen > 0){
      val uncompressedLen: Int = expectedPeakCount * bytePrecision * 2
      BytesUtils.uncompress(peaksBytes, compressedLen, uncompressedLen)
    }else{
      peaksBytes
    }
  }

}

