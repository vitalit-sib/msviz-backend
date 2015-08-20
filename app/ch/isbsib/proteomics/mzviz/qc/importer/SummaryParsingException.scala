package ch.isbsib.proteomics.mzviz.qc.importer

/**
 * Created by qjolliet on 03/08/15.
 */
case class SummaryParsingException( message:String,  cause:Throwable=null) extends Throwable(message, cause)
