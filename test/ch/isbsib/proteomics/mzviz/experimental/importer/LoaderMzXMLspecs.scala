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

    val mzXmlFile = new File("/Users/admin/Work/PAF/msViz/data/phosphoViz/mzXML/F001644.mzXML")
    val msnSpectraList = LoaderMzXML.parseFile(mzXmlFile)

    """check file""" in {
      mzXmlFile.getAbsolutePath mustEqual "/Users/admin/Work/PAF/msViz/data/phosphoViz/mzXML/F001644.mzXML"
    }

    """check size""" in {
      msnSpectraList.size mustEqual 13989
    }

    """check ms-level""" in {
      val ms2 = msnSpectraList.filter(_.getMsLevel > 1)

      println(ms2(0).getRetentionTimes)
      println(ms2(0).getParentScanNumber)
      println(ms2(0).getScanNumbers)
      println(ms2(0).getSpectrumIndex)

      val ints = ms2(0).getIntensities(null)
      println(ints.length)

      ms2.length mustEqual 1822
    }

    """check precursor""" in {

      val scanNumber17 = new ScanNumberDiscrete(17)
      val scanNumber18 = new ScanNumberDiscrete(18)
      val nr17 = msnSpectraList.filter(_.getSpectrumIndex == 17)
      val childrenNr17 = msnSpectraList.filter(_.getParentScanNumber == scanNumber17)

      println(nr17.length)
      println(nr17(0).getAnnotationIndexes.length)
      println(childrenNr17.length)

      println("parent 17")

      childrenNr17.foreach({
        one =>
          println("idx: " + one.getSpectrumIndex)
          println("nr: " + one.getScanNumbers)
          println("parent: " + one.getParentScanNumber)
          println("rt: " + one.getRetentionTimes)
          println("mslevel: " + one.getMsLevel)
          println("prec: " + one.getPrecursor.getMz)
          println("#annot: " + one.getAnnotationIndexes.length)
      })

      val nr18 = childrenNr17(0)

      println(nr18.getSpectrumIndex)
      println("precursor:")
      println(nr18.getPrecursor.getMz)
      println(nr18.getPrecursor.getIntensity)
      println(nr18.getPrecursor.getCharge)


//
//      println(nr17.get.getSpectrumIndex)
//      println(nr18.get.getSpectrumIndex)
//
//      println(nr17.get.getMsLevel)
//      println(nr18.get.getMsLevel)
//
//      println(nr17.get.getIntensities(null).length)
//      println(nr18.get.getIntensities(null).length)

      nr17.length mustEqual 1
    }


  }

}
