package ch.isbsib.proteomics.mzviz.experimental.importer



import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import org.expasy.mzjava.core.ms.spectrum.{ScanNumberDiscrete, MsnSpectrum}
import org.specs2.mutable.Specification
import scala.util.Success
import java.io.{ObjectOutputStream, FileOutputStream, File}

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMzXMLspecs extends Specification {

  "load MzXML" should {

    val mzXmlFile = new File("test/resources/ms1/F001644_small.mzXML")
    val ms1Iterator = LoaderMzXML.parseFile(mzXmlFile, RunId("hoho"))
    val sp = ms1Iterator.next

    """check size""" in {
      ms1Iterator.size mustEqual 97
    }

    """check scanNumber""" in {
      sp.spId.id mustEqual SpectrumUniqueId("1")
    }

    """check retentionTime""" in {
      sp.retentionTime mustEqual RetentionTime(0.176703)
    }

    """check peaks""" in {
      sp.peaks.length mustEqual 388
    }

    """check base peak""" in {
      val basePeak = sp.peaks.maxBy(_.intensity.value)
      basePeak.intensity mustEqual(Intensity(6809045.0))
      basePeak.moz mustEqual(Moz(519.1379352044135))
    }

  }

}
