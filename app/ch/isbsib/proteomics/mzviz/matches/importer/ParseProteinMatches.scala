package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.{SpectrumIdentifictionItem, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.matches.{SearchId, HitRank}
import ch.isbsib.proteomics.mzviz.matches.models._
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, ProteinIdentifier, AccessionCode}
import org.apache.commons.io.FilenameUtils

import scala.xml.{Elem, NodeSeq}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object ParseProteinMatches {

  val CvParamSpectrumTitle = "MS:1000796"
  val CvParamMascotScore = "MS:1001171"
  val CvDistinctPeptidesSequences = "MS:1001097"

  def parseProtList(mzidXml: Elem, searchId: SearchId, searchDbSourceInfo: Seq[SearchDatabase]): Seq[ProteinIdent] = {

    val proteinAmbiguityGroupList = mzidXml \\ "ProteinDetectionList" \\ "ProteinAmbiguityGroup"

    proteinAmbiguityGroupList.map({ onePAG =>
        val protDectList = (onePAG \\ "ProteinDetectionHypothesis").map({ oneDH =>
          val passThreshold = if(( oneDH \\ "@passThreshold").text == "true") true else false

          val nrPsms = ( oneDH \\ "PeptideHypothesis").size

          val mascotCv = (oneDH \\ "cvParam").find(_.attributes.exists(_.value.text == CvParamMascotScore))
          val mascotScore = (mascotCv.get \\ "@value").text
          val score = IdentScore(mascotScore.toDouble, Map())

          val dpsCv = (oneDH \\ "cvParam").find(_.attributes.exists(_.value.text == CvDistinctPeptidesSequences))
          val dps = (dpsCv.get \\ "@value").text.toInt

          val dbSeq = ( oneDH \\ "@dBSequence_ref").text
          val acAndDbSeqId = convertDbSeqId(dbSeq, searchDbSourceInfo).get

          ProteinIdentInfo(acAndDbSeqId._1, acAndDbSeqId._2, score, dps, nrPsms, passThreshold)
        })
        // the first proteinDetection is the main one
        ProteinIdent(searchId, protDectList(0), protDectList.tail)
      })

  }


  def convertDbSeqId(dbSeqId: String, searchDbSourceInfo: Seq[SearchDatabase]): Option[Tuple2[AccessionCode, SequenceSource]] = {
    // we assume that the name DB names will always be coherent
    val pattern = "DBSeq_(\\d+)_(.+)".r

    pattern.findFirstMatchIn(dbSeqId).map({ oneMatch =>
      // we assume that the order of the databases stays always the same in an MzId export
      val seqSource = searchDbSourceInfo(oneMatch.group(1).toInt - 1)
      Tuple2(AccessionCode(oneMatch.group(2)), SequenceSource(seqSource.id))
    })

  }


  /**
   * parse the relation between spectrumId and spectrumTitle from MzId.
   * This information is used to create link between ProteinList and PTM's
   *
   * @param spIdList
   * @return
   */
  def parseSpectrumIdAndTitleRelation(spIdList: NodeSeq): Map[SpectrumIdentifictionItem, SpectrumUniqueId] = {

    (spIdList \\ "SpectrumIdentificationResult").flatMap({spIdRes =>
      val titleCv = (spIdRes \\ "cvParam").find(_.attributes.exists(_.value.text == CvParamSpectrumTitle))
      val title=(titleCv.get \\ "@value").text
      val reTitleScan = """.*\.(\d+)\.\d$""".r
      val scanNumber =title match {
        case reTitleScan(s) => s
        case _ => throw new Exception("cannot parse scan number from " + title)
      }

      val spTitle = SpectrumUniqueId(scanNumber)

      (spIdRes \\ "SpectrumIdentificationItem").map({spId =>
        //val hitRank = HitRank((spId \\ "@rank").text.toInt)
        val spIdItem = SpectrumIdentifictionItem((spId \\ "@id").text)
        (spIdItem -> spTitle)
      })
    }).toMap

  }

}
