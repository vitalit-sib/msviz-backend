package ch.isbsib.proteomics.mzviz.experimental.importer

import ch.isbsib.proteomics.mzviz.experimental.{RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.models._
import org.scalacheck.Prop.Exception
import uk.ac.ebi.jmzml.model.mzml._
import uk.ac.ebi.jmzml.xml.io.{MzMLObjectIterator, MzMLUnmarshaller}
import java.io.File

import scala.collection.JavaConverters._
import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.commons.helpers.CommonFunctions
import ch.isbsib.proteomics.mzviz.experimental.ScanNumber


/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2017, SIB Swiss Institute of Bioinformatics
 */


/**
 * parse an MzML file using the EBI parser
 */
class LoaderMzML {

  /**
   *
   * @param mzmlFile
   * @return
   */
  def parse(mzmlFile: File, runId: RunId): MzMLIterator ={

    val xpath:String  = "/run/spectrumList/spectrum"
    val unmarshaller = new MzMLUnmarshaller(mzmlFile)
    val it:MzMLObjectIterator[Nothing] = unmarshaller.unmarshalCollectionFromXpath(xpath, classOf[Spectrum])

    new MzMLIterator(it, runId)
  }

}


/**
 * Transform the MzMLObjectIterator from EBI to our own iterator
 *
 * @param mzMLObjectIterator
 */

class MzMLIterator(mzMLObjectIterator: MzMLObjectIterator[Nothing], runId: RunId) extends Iterator[Either[ExpMs1Spectrum, ExpMSnSpectrum]]{

  val scanNumberPattern = """scan=(\d+)""".r

  /**
   * get the next element from the EBI parser and transform it to either a ms1 or msn spectrum
   * @return
   */
  def next(): Either[ExpMs1Spectrum, ExpMSnSpectrum] = {
    val sp:Spectrum = mzMLObjectIterator.next().asInstanceOf[Spectrum]

    val cvParams:Seq[CVParam] = sp.getCvParam.asScala.toSeq

    // CV accession for ms-level is MS:1000511
    val msLevel = parseCvEntry(cvParams, "MS:1000511").get.toInt

    if(msLevel == 1) Left(parseMs1(sp, runId))
    else Right(parseMsN(sp, runId))

  }

  /**
   * check if there is a next element
   * @return
   */
  def hasNext():Boolean = {
    mzMLObjectIterator.hasNext
  }


  /**
   * return the CV value for a given accession
   *
   * @param cvParams
   * @param accession
   * @return
   */
  def parseCvEntry(cvParams: Seq[CVParam], accession: String): Option[String] = {

    val fltCv = cvParams.filter(_.getAccession == accession)
    if(fltCv.length == 1) Some(fltCv(0).getValue)
    else None
  }


  /**
   * convert a binaryData to a list of peaks
   * @param binaryData
   * @return
   */
  def parseBinaryData(binaryData: Seq[BinaryDataArray]):Array[(Moz, Intensity)] = {
    if(binaryData.size != 2) throw new IllegalStateException("There should be only 2 entries in list: one for moz and one for int")

    val mzs: Array[Number] = binaryData(0).getBinaryDataAsNumberArray()
    val intensities: Array[Number] = binaryData(1).getBinaryDataAsNumberArray()

    (mzs).zip(intensities).map(p => (Moz(p._1.doubleValue), Intensity(p._2.doubleValue)))
  }


  /**
   * transfrom a spectrum to a ExpMs1Spectrum (MS1 data)
   * @param sp
   * @param runId
   * @return
   */
  def parseMs1(sp:Spectrum, runId: RunId):ExpMs1Spectrum = {
    val scanNr:ScanNumber = ScanNumber(scanNumberPattern.findFirstIn(sp.getId).get.split("=")(1).toInt)
    val spId: SpectrumId = SpectrumId(SpectrumUniqueId(scanNr.value.toString), runId)

    // parse info from scanList
    if(sp.getScanList.getCount != 1) throw new IllegalStateException("None or more than one scan found: Don't know how to handle that")
    val scanListCvParams = sp.getScanList.getScan.get(0).getCvParam.asScala
    // it looks like the rt is in minutes
    val rt: RetentionTime = RetentionTime(parseCvEntry(scanListCvParams, "MS:1000016").get.toDouble * 60)

    val rawPeaks = parseBinaryData(sp.getBinaryDataArrayList.getBinaryDataArray.asScala)
    val peaks: List[ExpPeakMS1] = rawPeaks.map(p => ExpPeakMS1(p._1, p._2)).toList

    ExpMs1Spectrum(spId, rt, Some(scanNr), peaks)
  }

  /**
   * transform a spectrum to a ExpMSnSpectrum (MS2+ data)
   * @param sp
   * @param runId
   * @return
   */
  def parseMsN(sp:Spectrum, runId: RunId):ExpMSnSpectrum = {
    val spCvParams:Seq[CVParam] = sp.getCvParam.asScala

    val scanNr:ScanNumber = ScanNumber(scanNumberPattern.findFirstIn(sp.getId).get.split("=")(1).toInt)
    val msLevel = parseCvEntry(spCvParams, "MS:1000511").get.toInt
    val spTitle = parseCvEntry(spCvParams, "MS:1000796").get
    val spId: SpectrumId = SpectrumId(SpectrumUniqueId(scanNr.value.toString), runId)

    // parse info from scanList
    if(sp.getScanList.getCount != 1) throw new IllegalStateException("None or more than one scan found: Don't know how to handle that")
    val scanListCvParams = sp.getScanList.getScan.get(0).getCvParam.asScala.toSeq
    val precRt: RetentionTime = RetentionTime(parseCvEntry(scanListCvParams, "MS:1000016").get.toDouble * 60)

    // parse precursor info
    if(sp.getPrecursorList.getCount != 1) throw new IllegalStateException("None or more than one precursor found: Don't know how to handle that")
    val ebiPrec = sp.getPrecursorList.getPrecursor.get(0)
    val selIonList = ebiPrec.getSelectedIonList.getSelectedIon
    if(selIonList.size() != 1) throw new IllegalStateException("None or more than one selected ion found: Don't know how to handle that")

    // parse precursor CV
    val ebiPrecCvs:Seq[CVParam] = selIonList.get(0).getCvParam.asScala
    val precMoz:Moz = Moz(parseCvEntry(ebiPrecCvs, "MS:1000744").get.toDouble)
    val precIntensitiy:Intensity = Intensity(parseCvEntry(ebiPrecCvs, "MS:1000042").getOrElse("0").toDouble)
    val precCharge = Charge(parseCvEntry(ebiPrecCvs, "MS:1000041").get.toInt)
    val precScanNr:ScanNumber = ScanNumber(scanNumberPattern.findFirstIn(ebiPrec.getSpectrumRef).get.split("=")(1).toInt)

    //Calculate molecularMass if possible
    val molMass = if(precMoz.value !=0 && precCharge.value !=0) (Some(MolecularMass((precMoz.value * precCharge.value) - (CommonFunctions.PROTON_MASS * precCharge.value)))) else None
    // if its directly from the raw file we don't set a source
    val molMassSource = None
    // create precursor
    val precursor:ExpPeakPrecursor = ExpPeakPrecursor(precMoz, precIntensitiy, precRt, precCharge, Some(precScanNr), molMass, molMassSource)

    // ref spectrum info
    val ref:SpectrumRef = SpectrumRef(Some(scanNr), precursor, spTitle, spId)

    // parse peak info
    val rawPeaks = parseBinaryData(sp.getBinaryDataArrayList.getBinaryDataArray.asScala)

    // get intensityRanks
    val ordererIndexes = rawPeaks.map(_._2.value).zip(0 to rawPeaks.size-1).sortBy(_._1).map(_._2)
    val intensityRank = ordererIndexes.zip(rawPeaks.size-1 to 0 by -1).sortBy(_._1).map(_._2)

    // create list of peaks
    val peaks: List[ExpPeakMSn] = rawPeaks.zip(intensityRank).map(p => ExpPeakMSn(p._1._1, p._1._2, IntensityRank(p._2), MSLevel(msLevel))).toList

    ExpMSnSpectrum(ref, peaks)
  }

}



/**
 * the companion object
 */

object LoaderMzML {

  def apply() = new LoaderMzML

}