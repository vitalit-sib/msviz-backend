package ch.isbsib.proteomics.mzviz.results.basket

import ch.isbsib.proteomics.mzviz.commons.{Intensity, RetentionTime}
import ch.isbsib.proteomics.mzviz.results.basket.models.BasketEntryWithSpInfo

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object BasketTsvFormat {

  def toTsv(basketEntries: Seq[BasketEntryWithSpInfo]): String = {

    val sb = new StringBuilder

    // construct the header
    val searchIdsList:Seq[String] = basketEntries(0).searchIds.split(",")
    val idHeaderFields = searchIdsList.map({ sId =>
      Seq(sId + " retentionTime", sId + " intensity")
    }).flatten

    val headerFields: List[String] = List[String]("proteinAC", "peptideSequence", "startPos", "endPos", "runId", "spectrumId",
                                      "scanNumber", "precRt", "precMoz", "precCharge", "mascotScore",
                                      "localizationScore") ++ idHeaderFields

    // extract the values
    def extractVal: ((BasketEntryWithSpInfo) => List[Any]) = { b: BasketEntryWithSpInfo =>
      List(b.proteinAC.value, b.prevAA.getOrElse("") + "." + b.peptideSeq + "." + b.nextAA.getOrElse(""), b.startPos, b.endPos, b.spectrumId.runId.value, b.spectrumId.id.value,
        b.scanNr, b.precRt, b.precMoz, b.precCharge, b.score, b.localizationScore.getOrElse("")) ++
        b.xicPeaks.map(p => Seq(rtToString(p.rt), intToString(p.intensity))).flatten
    }

    sb.append(headerFields.mkString("\t") + "\n")
    basketEntries.foreach({ b =>
      sb.append(extractVal(b).mkString("\t") + "\n")
    })
    sb.toString()
  }


  // helper function to extract Rt and intensities
  def rtToString(rt: Option[RetentionTime]):String = {
    rt match {
      case Some(x) => x.value.toString
      case None => ""
    }
  }

  def intToString(rt: Option[Intensity]):String = {
    rt match {
      case Some(x) => x.value.toString
      case None => ""
    }
  }



}
