package ch.isbsib.proteomics.mzviz.theoretical.services

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpMSnSpectrum, RefSpectrum, ExpPeakMSn, ExpPeakPrecursor}
import ch.isbsib.proteomics.mzviz.experimental.{IdRun, ScanNumber}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import ch.isbsib.proteomics.mzviz.theoretical.models.FastaEntry
import play.api.libs.json._

/**
 * Created by tmartinc on 05/12/14.
 * @author Trinidad Martin
 */
object JsonTheoFormats {

  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._


//  implicit val formatAccessionCode = new Format[AccessionCode] {
//    override def reads(json: JsValue): JsResult[AccessionCode] = JsSuccess(AccessionCode(json.as[String]))
//
//    def writes(o: AccessionCode) = JsString(o.value)
//  }
  implicit val formatAC = Json.format[AccessionCode]
  implicit val formatFastaEntry = Json.format[FastaEntry]

}
