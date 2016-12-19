package ch.isbsib.proteomics.mzviz.experimental.importer



import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import org.specs2.mutable.Specification
import java.io.File

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMzXMLspecs extends Specification {

  "load MzXML" should {

    val mzXmlFile = new File("test/resources/ms1/F001644_small.mzXML")
    val ms1Iterator = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))
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

    """check total #peaks""" in {
      var total = 0
      val ms1Iterator2 = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))
      while(ms1Iterator2.hasNext){
        val sp = ms1Iterator2.next
        total += sp.peaks.length
      }
      total mustEqual 31771
    }

  }

  "load MzXML2" should {

    val mzXmlFile = new File("test/resources/ms1/F001644_small.mzXML")
    val ms1Sps = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))

    """check size""" in {
      ms1Sps.size mustEqual 98
    }

    """check scanNumber""" in {
      val ms1Sps = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))
      ms1Sps.next.spId.id mustEqual SpectrumUniqueId("1")
    }

    """check retentionTime""" in {
      val ms1Sps = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))
      ms1Sps.next.retentionTime mustEqual RetentionTime(0.176703)
    }

    """check peaks""" in {
      val ms1Sps = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))
      ms1Sps.next.peaks.length mustEqual 388
    }

    """check base peak""" in {
      val ms1Sps = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))
      val basePeak = ms1Sps.next.peaks.maxBy(_.intensity.value)
      basePeak.intensity mustEqual(Intensity(6809045.0))
      basePeak.moz mustEqual(Moz(519.1379352044135))
    }

    """check total #peaks""" in {
      val ms1Sps = LoaderMzXML().parse(mzXmlFile, RunId("hoho"))
      val totalPeaks = ms1Sps.map(_.peaks.length).sum
      totalPeaks mustEqual 31771
    }

  }



}
