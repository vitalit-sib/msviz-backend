package ch.isbsib.proteomics.mzviz.modifications.importer

import scala.collection.immutable.Seq
import scala.xml.XML

/**
 * opens a file and get a list of modifications
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class UnimodParser (file:String) {
  //val filename="http://mascot.vital-it.ch/mascot/cgi/get_params.pl?Show=MS_UNIMODXML"
  val unimodfile = XML.load(file)

  val key=(unimodfile \\ "mod").map { mod => (mod \ "@full_name").text}
  val tuple=(unimodfile \\ "mod").map { mod =>
    (mod \\ "delta").map { delta => Tuple2((delta \ "@mono_mass").text, (delta \ "@avge_mass").text)}
  }

  val dictionary=(key zip tuple).toMap

  /**
   * return number of entries
   * @return
   */
  def getSize:Int ={
    dictionary.size
  }

  /**
   * return value for a given key
   * @return
   */
  def getValues(key:String):Option[Seq[(String, String)]] = {
    dictionary.get(key)
  }

}

/**
 * companion object
 */
object UnimodParser {
  def apply(filename: String) = new UnimodParser(filename)

}