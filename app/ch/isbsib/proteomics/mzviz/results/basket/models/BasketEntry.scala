package ch.isbsib.proteomics.mzviz.results.basket.models

import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import ch.isbsib.proteomics.mzviz.commons.services.MongoId
import ch.isbsib.proteomics.mzviz.experimental.ScanNumber
import java.util.Date

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


trait BasketEntryBase {
  def _id: Option[MongoId]
  def proteinAC: AccessionCode
  def peptideSeq: String
  def startPos: Int
  def endPos: Int
  def searchIds: String
  def spectrumId: SpectrumId
  def score: Double
  def localizationScore: Option[Double]
  def ppmTolerance: Double
  def rtZoom: RtRange
  def rtSelected: RtRange
  def xicPeaks: Seq[XicPeak]
  def creationDate: Option[Date]
}


/**
 *
 * A BasketEntry contains all the result information needed.
 *
 * @param proteinAC
 * @param peptideSeq annotated peptide sequence
 * @param startPos
 * @param endPos
 * @param searchIds comma separated searchIds
 * @param spectrumId
 * @param ppmTolerance
 * @param rtZoom
 * @param rtSelected
 * @param creationDate
 */
case class BasketEntry (_id: Option[MongoId], proteinAC: AccessionCode, peptideSeq: String, startPos: Int, endPos: Int, searchIds: String,
                        spectrumId: SpectrumId, score: Double, localizationScore: Option[Double], ppmTolerance: Double, rtZoom: RtRange,
                        rtSelected: RtRange, xicPeaks: Seq[XicPeak], creationDate: Option[Date]) extends BasketEntryBase


/**
 *
 * A BasketEntry with additional spectrumInfos
 *
 * @param proteinAC
 * @param peptideSeq annotated peptide sequence
 * @param startPos
 * @param endPos
 * @param searchIds comma separated searchIds
 * @param spectrumId
 * @param ppmTolerance
 * @param rtZoom
 * @param rtSelected
 * @param creationDate
 */
case class BasketEntryWithSpInfo (_id: Option[MongoId], proteinAC: AccessionCode, peptideSeq: String, startPos: Int, endPos: Int, searchIds: String,
                        spectrumId: SpectrumId, scanNr:ScanNumber, precRt: Double, precCharge: Int, precMoz: Double, score: Double, localizationScore: Option[Double], ppmTolerance: Double, rtZoom: RtRange,
                        rtSelected: RtRange, xicPeaks: Seq[XicPeak], creationDate: Option[Date]) extends BasketEntryBase

/**
 * A class keeping a retention time range.
 *
 * @param lowerRt
 * @param upperRt
 */
case class RtRange(lowerRt: Double, upperRt: Double)


/**
 * The quantitation result for a selected XIC region.
 * For the moment the intensity corresponds only to the most intense point in the selected region and
 * its exact retention time.
 *
 * @param searchId
 * @param rt
 * @param intensity
 */
case class XicPeak(searchId: SearchId, rt: Option[RetentionTime], intensity: Option[Intensity])



