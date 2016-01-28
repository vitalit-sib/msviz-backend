package ch.isbsib.proteomics.mzviz.experimental.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental._
import ch.isbsib.proteomics.mzviz.experimental.models.ExpMSnSpectrum
import org.specs2.mutable.Specification

import scala.util.Success

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMGFSpecs extends Specification {
  "textLine2MozIntensity" should {
    """get ("123.45 456.67") """ in {
      LoaderMGF.textLine2MozIntensity(Some("123.45 456.67")) mustEqual Success(Tuple2(Moz(123.45), Intensity(456.67)))
    }

    """get ("123.45 45E+2",23) """ in {
      LoaderMGF.textLine2MozIntensity(Some("123.45 45E+2")) mustEqual Success(Tuple2(Moz(123.45), Intensity(4500)))
    }
    """get ("123.45) """ in {
      LoaderMGF.textLine2MozIntensity(Some("123.45")) mustEqual Success(Tuple2(Moz(123.45), Intensity(0)))
    }

    """get "123 xyz"  """ in {
      LoaderMGF.textLine2MozIntensity(Some("123 xyz")).isFailure mustEqual true
    }
  }

  "text2peaks" should {
    "do fine" in {
      val txt =
        """
          |BEGIN IONS
          |PIPO=coincoi
          |123 345
          |123.4E+2 567
          |567
          |END IONS
        """
          .stripMargin
      val peaks = LoaderMGF.text2peaks(txt).get
      peaks must have size 3

      peaks(2).intensity must equalTo(Intensity(567.0))
      peaks(2).intensityRank must equalTo(IntensityRank(1))
    }

  }

  val microblock =
    """
      |BEGIN IONS
      |CHARGE=2+
      |PEPMASS=562.259301
      |RTINSECONDS[0]=3041.979
      |TITLE=20141008_BSA_25cm_column2.10988.10988.2
      |136.075546 2567
      |299.060120 1042
      |359.023926 849.8
      |447.345581 924.3
    """.stripMargin

  "text2map" should {
    val args = LoaderMGF.text2map(microblock)
    "get PEPTMASS" in {
      args.get("PEPMASS") must equalTo(Some("562.259301"))

    }
    "miss PIPO" in {
      args.get("PIPO") must equalTo(None)
    }
  }

  "text2Precursor" should {
    "get it" in {
      val prec = LoaderMGF.text2Precursor(microblock, RunId("PAF")).get

      prec.precursor.charge must equalTo(Charge(2))

      prec.title must equalTo("20141008_BSA_25cm_column2.10988.10988.2")

    }
  }

  "load" should {

    "count the msms" in {
      val run: Iterator[ExpMSnSpectrum] = LoaderMGF.load(new File("test/resources/mascot/F001644.mgf"), RunId("pipo")).get
      run.size must equalTo(1822)
    }
    "check a guy" in {

      val run: Iterator[ExpMSnSpectrum] = LoaderMGF.load(new File("test/resources/mascot/F001644.mgf"), RunId("pipo")).get

      val sp = run.toSeq(3)
      sp.ref.precursor.charge must equalTo(Charge(2))
      sp.ref.precursor.moz must equalTo(Moz(357.235892))
      sp.ref.precursor.intensity must equalTo(Intensity(538655.5))
      sp.ref.precursor.retentionTime must equalTo(RetentionTime(2999.76954))
      sp.ref.title must equalTo("20141008_BSA_25cm_column2.10823.10823.2")
      sp.ref.scanNumber must equalTo(ScanNumber(10823))
    }
    "m/z are increasing order" in {
      val run: Iterator[ExpMSnSpectrum] = LoaderMGF.load(new File("test/resources/mascot/F001644.mgf"), RunId("pipo")).get
      val mozs = run.toSeq(0).peaks.map(_.moz).toList
      val mDelta = mozs.drop(1).zip(mozs.dropRight(1)).map (p => p._1.value - p._2.value).filter(_<0)
      mDelta must have size(0)
    }
  }

  "loading wiff" should {

    "count the msms" in {
      val run = LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("pipo")).get
      run.toSeq.size must equalTo(123)
    }
    "check a guy" in {
      val run = LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("pipo")).get
      val sp = run.toSeq(3)
      sp.ref.precursor.charge must equalTo(Charge(2))
      sp.ref.precursor.moz must equalTo(Moz(407.717649))
      sp.ref.precursor.intensity must equalTo(Intensity(0))
      sp.ref.precursor.retentionTime must equalTo(RetentionTime(49.866 * 60))
      sp.ref.title must equalTo("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 49.866 min, Period: 1, Cycle(s): 2030 (Experiment 4)")
      sp.ref.scanNumber must equalTo(ScanNumber(-1))

    }
    "args2RT in elution time point in TITLE" in {
      val map: Map[String, String] =Map("TITLE"-> """File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 49.866 min, Period: 1, Cycle(s): 2030 (Experiment 4)""")
      val rt = LoaderMGF.args2RT(map)
      rt must equalTo(Success(RetentionTime(49.866 * 60)))
    }
    "args2RT in elution time interval in TITLE" in {
      val map: Map[String, String] =Map("TITLE"-> """File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 49.454 to 49.574 min, Period: 1, Cycle(s): 2027-2028 (Experiment 3), 2027 (Experiment 4)""")
      val rt = LoaderMGF.args2RT(map)
      rt must equalTo(Success(RetentionTime(30*(49.454 + 49.574))))
    }


  }

  "id should be scanNumber" in{
    val runMascot = LoaderMGF.load(new File("test/resources/mascot/F001644.mgf"), RunId("pipo")).get
    val firstSpectra=runMascot.next()

    firstSpectra.ref.spectrumId.id.value mustEqual(firstSpectra.ref.scanNumber.value.toString)
    firstSpectra.ref.spectrumId.id.value mustEqual("11150")

    val runMaxQuant = LoaderMGF.load(new File("test/resources/maxquant/8077A.mgf"), RunId("pipoMax")).get

    val firstSpectraMQ=runMaxQuant.next()

    firstSpectraMQ.ref.spectrumId.id.value mustEqual(firstSpectraMQ.ref.scanNumber.value.toString)
    firstSpectraMQ.ref.spectrumId.id.value mustEqual("4")

  }
}
