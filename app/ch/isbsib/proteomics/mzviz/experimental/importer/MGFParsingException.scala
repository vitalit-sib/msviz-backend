package ch.isbsib.proteomics.mzviz.experimental.importer

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class MGFParsingException( message:String,  cause:Throwable=null) extends Throwable(message, cause)