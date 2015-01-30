package ch.isbsib.proteomics.mzviz.modifications.importer

import ch.isbsib.proteomics.mzviz.theoretical.{NumDatabaseSequences, SequenceSource}
import scala.xml.XML

/**
 * opens a file and get a list of modifications
 *
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 * TODO: import file Unimod.xml from Mascot server  "http://mascot.vital-it.ch/mascot/cgi/get_params.pl?Show=MS_UNIMODXML"
 */
class UnimodParser {
  //val filename="test/resources/unimod.xml"
  val unimod = XML.load("http://mascot.vital-it.ch/mascot/cgi/get_params.pl?Show=MS_UNIMODXML")

  val key=(unimod \\ "mod").map { mod => (mod \ "@full_name").text}
  val tuple=(unimod \\ "mod").map { mod =>
    (mod \\ "delta").map { delta => Tuple2((delta \ "@mono_mass").text, (delta \ "@avge_mass").text)}
  }

  val dictionary=(key zip tuple).toMap

}