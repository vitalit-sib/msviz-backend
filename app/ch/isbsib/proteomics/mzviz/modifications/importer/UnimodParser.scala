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
  val mod=(unimodfile \\ "mod")
  val modificationList=obtainModifications
  val dictionary=modificationList.toMap


    /**
     * return list of modifications
     * @return
     */
    def obtainModifications: Seq[(String,Double)]={

      val l = for {m <- mod}
      yield
      {
        val modificationName= (m\ "@full_name").text
        val mono_mass=(m \ "delta"  \\ "@mono_mass").text
        ( modificationName, mono_mass.toDouble)
      }
      l
    }

  /**
   * return size of the dictionary
   * @return
   */

  def getDictionarySize: Int= {
    dictionary.size
  }

  /**
   * return value for a given key
   * @return
   */

  def getValue (key:String):Option[Double]= {
    dictionary.get(key)
  }

  /**
   * return dictionary
   * @return
   */

  def getDictionary = {
    dictionary
  }
}

/**
 * companion object
 */
object UnimodParser {
  def apply(filename: String) = new UnimodParser(filename)
}