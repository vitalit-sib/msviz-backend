package ch.isbsib.proteomics.mzviz.qc.services

import ch.isbsib.proteomics.mzviz.qc.models.{QcSummaryEntry, RawfileInfomation}

/**
 * Created by Qinfang Jolliet on 05/12/14.
 * @author Roman Mylonas, Trinidad Martin  & Qinfang Jolliet & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object JsonQCFormats {

  import play.api.libs.json.Json

  implicit val formatRawFileInformation =  Json.format[RawfileInfomation]
  implicit val formatSummary =  Json.format[QcSummaryEntry]

}
