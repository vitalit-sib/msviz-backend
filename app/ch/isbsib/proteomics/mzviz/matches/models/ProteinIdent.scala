package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */


/**
 * Information on the proteins parsed from MzId file.
 * This class is used to populate the list of proteins found in a search.
 * @param searchId
 * @param mainProt
 * @param subsetProts
 */
case class ProteinIdent(searchId: SearchId, mainProt: ProteinIdentInfo, subsetProts: Seq[ProteinIdentInfo])

/**
 * Information on possible protein subsets of a protein identification.
 *
 * @param proteinAC
 * @param source
 * @param score
 * @param nrSequences
 * @param nrPsms
 */
case class ProteinIdentInfo(proteinAC: AccessionCode, source: SequenceSource, score: IdentScore, nrSequences: Int, nrPsms: Int, passThreshold: Boolean)

