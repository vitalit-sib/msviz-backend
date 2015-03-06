package ch.isbsib.proteomics.mzviz.experimental.importer

/**
 * Created by amasselo on 3/6/15.
 */
case class MGFParsingException( message:String,  cause:Throwable=null) extends Throwable(message, cause)