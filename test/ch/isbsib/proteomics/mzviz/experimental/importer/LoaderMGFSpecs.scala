package ch.isbsib.proteomics.mzviz.experimental.importer

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.models.ExpPeakMSn
import org.specs2.mutable.Specification
import ch.isbsib.proteomics.mzviz.experimental._

import scala.util.Success

/**
 * @author Alexandre Masselot
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

      peaks(1).intensity must equalTo(Intensity(567.0))
      peaks(1).intensityRank must equalTo(IntensityRank(1))
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
    val run = LoaderMGF.load("test/resources/F001644.mgf")
    "get runId out of filename" in {
      run.id must equalTo(RunId("F001644"))
    }
    "count the msms" in {
      run.msnSpectra.size must equalTo(1822)
    }
    "check a guy" in {
      /*
            BEGIN IONS
              CHARGE=2+
              PEPMASS=357.235892 538655.5
            RTINSECONDS[0]=2999.76954
            TITLE=20141008_BSA_25cm_column2.10823.10823.2
            133.085892 8.902e+04
            287.006805 4.126e+04
            325.211578 5.535e+04
      */
      val sp = run.msnSpectra(3)
      sp.ref.precursor.charge must equalTo(Charge(2))
      sp.ref.precursor.moz must equalTo(Moz(357.235892))
      sp.ref.precursor.intensity must equalTo(Intensity(538655.5))
      sp.ref.precursor.retentionTime must equalTo(RetentionTime(2999.76954))
      sp.ref.title must equalTo("20141008_BSA_25cm_column2.10823.10823.2")
      sp.ref.scanNumber must equalTo(ScanNumber(10823))
    }
  }

  "loading wiff" should {
    val run = LoaderMGF.load("test/resources/M_100.mgf")

    "get runId out of filename" in {
      run.id must equalTo(RunId("M_100"))
    }
    "count the msms" in {
      run.msnSpectra.size must equalTo(123)
    }
    "check a guy" in {
      /*
        BEGIN IONS
        CHARGE=2+
        PEPMASS=407.717649
        TITLE=File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 49.866 min, Period: 1, Cycle(s): 2030 (Experiment 4)
        196.114300 2.518
        287.136800 1.85
        409.147600 3.974
        476.238900 1.148
      */
      val sp = run.msnSpectra(3)
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
}
