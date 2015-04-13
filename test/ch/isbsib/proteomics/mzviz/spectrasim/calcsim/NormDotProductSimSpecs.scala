package ch.isbsib.proteomics.mzviz.spectrasim.calcsim

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.importer.LoaderMGF
import ch.isbsib.proteomics.mzviz.experimental.models._
import ch.isbsib.proteomics.mzviz.experimental.{MSRun, RunId, ScanNumber, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.spectrasim.models.SpSpMatch
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class NormDotProductSimSpecs extends Specification {

  val peakMatchTol = 0.5

  "compute normalized dot product" should {

    val mzs = Seq(532.2181757006471, 592.0813696890917, 610.4389304593608, 819.528204667349, 1008.9731541854521, 1114.2026733872658, 1228.8277418928208, 1250.291877455562, 1458.4947369611507, 1617.97592028064, 1673.0009209859188, 1711.5858053298323, 1900.6064002325252, 1978.9769588099969, 2004.1818947649767, 2121.1340129945756, 2242.9549664843066, 2268.9075118303103, 2297.4721551530297, 2348.226927064944)
    val int1 = Seq(5.0, 8.0, 11.0, 7.0, 6.0, 21.0, 11.0, 2.0, 15.0, 14.0, 23.0, 4.0, 7.0, 18.0, 1.0, 26.0, 3.0, 24.0, 6.0, 12.0)
    val int2 = Seq(6.0, 9.0, 21.0, 20.0, 1.0, 23.0, 9.0, 1.0, 7.0, 26.0, 12.0, 16.0, 9.0, 5.0, 7.0, 2.0, 9.0, 9.0, 5.0, 28.0)

    // prepare spectra
    def prepSp(mzs: Seq[Double], ints: Seq[Double]): ExpMSnSpectrum ={
      val prec1 = ExpPeakPrecursor(Moz(100.1), Intensity(1000), RetentionTime(10.0), Charge(1))
      val spId1 = SpectrumId(SpectrumUniqueId("sp1"), RunId("run1"))
      val ref1 = SpectrumRef(ScanNumber(1), prec1, "sp1", spId1)
      val peaks1 = (mzs zip ints).map(p => ExpPeakMSn(Moz(p._1), Intensity(p._2), IntensityRank(0), MSLevel(0)))
      ExpMSnSpectrum(ref1, peaks1)
    }

    val sp1 = prepSp(mzs, int1)
    val sp2 = prepSp(mzs, int2)

    "compare two spectra" in {

      val spSpMatch = NormDotProdSim().calcSimilarity(sp1, sp2, peakMatchTol)

      spSpMatch.score must equalTo(0.7215238028982041)

    }
  }


  "compute normalized dot product from MGF" should {

    val run: MSRun = LoaderMGF.load(new File("test/resources/M_100.mgf"), RunId("pipo")).get

    "compare two spectra from MGF" in {

      val spSpMatches = NormDotProdSim().calcSimilarityList(run.msnSpectra(0), run.msnSpectra, peakMatchTol)

      // best match should be spectra 0
      def maxMatch(match1: SpSpMatch, match2: SpSpMatch): SpSpMatch = if(match1.score > match2.score) match1 else match2
      val bestMatch = spSpMatches.reduceLeft(maxMatch)

      run.msnSpectra(0) must equalTo(bestMatch.sp2)

      // we should have same number of matches as search spectra
      run.msnSpectra.length must equalTo(spSpMatches.length)

      // look at score of second match
      spSpMatches(1).score must equalTo(0.09029509137335748)

    }
  }
}
