package ch.isbsib.proteomics.mzviz.results.basket.models

import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

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
 */
case class BasketEntry (proteinAC: AccessionCode, peptideSeq: String, startPos: Int, endPos: Int, searchIds: String,
                        spectrumId: SpectrumId, ppmTolerance: Double, rtZoom: RtRange,
                        rtSelected: RtRange, xicPeaks: Seq[XicPeak])

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



