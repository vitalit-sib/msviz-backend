package ch.isbsib.proteomics.mzviz.experimental.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.RunId
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas & Trinidad Martin
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
  class LoaderMzMLSpecs extends Specification {

  "load MzXML" should {

    val mzMlFile = new File("test/resources/ms2/20160215_Fujita_8133B_subset.mzML")
    val spList = LoaderMzML().parse(mzMlFile, RunId("hoho")).toSeq


    """check size""" in {
      spList.size mustEqual 66
    }

    """check number ms2""" in {
      spList.filter(_.isRight).size mustEqual(41)
    }

    """check first MS2""" in {
      val spEither = spList(0)

      // should be ms2
      spEither.isRight mustEqual(true)

      // check spectrumId
      val sp = spEither.right.get
      sp.ref.scanNumber.get.value mustEqual(6381)
      sp.ref.title mustEqual("""20160215_Fujita_8133B.6381.6381.3 File:"20160215_Fujita_8133B.raw", NativeID:"controllerType=0 controllerNumber=1 scan=6381"""")
      sp.ref.spectrumId.id.value mustEqual("6381")
      sp.ref.spectrumId.runId.value mustEqual("hoho")

      // check precursor
      sp.ref.precursor.charge.value mustEqual(3)
      sp.ref.precursor.intensity.value mustEqual(9.612146875e05)
      sp.ref.precursor.moz.value mustEqual(738.696250136775)
      sp.ref.precursor.retentionTime.value mustEqual(35.157922)
      sp.ref.precursor.scanNumber.get.value mustEqual(6377)

      // check peaks
      sp.peaks.size mustEqual(486)
      val sortedPeaks = sp.peaks.sortBy(_.intensity.value).reverse
      val highestPeak = sortedPeaks(0)
      highestPeak.intensity.value mustEqual(5.28457375e05)
      highestPeak.intensityRank.value mustEqual(0)
      highestPeak.moz.value mustEqual(738.3854370117188)

      val lowestPeak = sortedPeaks.last
      lowestPeak.intensity.value mustEqual(628.7720947265625)
      lowestPeak.moz.value mustEqual(365.1828918457031)
      lowestPeak.intensityRank.value mustEqual(485)

    }

    """check first MS1""" in {
      val spEither = spList.filter(_.isLeft)(0)

      // should be ms1
      spEither.isLeft mustEqual(true)

      // check spectrumId
      val sp = spEither.left.get
      sp.spId.id.value mustEqual("6385")
      sp.spId.runId.value mustEqual("hoho")

      // check scanNr and rt
      sp.scanNumber.get.value mustEqual(6385)
      sp.retentionTime.value mustEqual(2110.2756)

      // check peaks
      sp.peaks.size mustEqual(5132)
      val sortedPeaks = sp.peaks.sortBy(_.intensity.value).reverse
      val highestPeak = sortedPeaks(0)
      highestPeak.intensity.value mustEqual(8.44145152E8)
      highestPeak.moz.value mustEqual(425.73749341744343)

      val lowestPeak = sortedPeaks.last
      lowestPeak.intensity.value mustEqual(0.0)
      lowestPeak.moz.value mustEqual(391.07122584902334)

    }

  }

}
