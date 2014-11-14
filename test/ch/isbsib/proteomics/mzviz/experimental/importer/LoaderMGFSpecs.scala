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
    """get ("123.45 456.67",None) """ in {
      LoaderMGF.textLine2MozIntensity(Some("123.45 456.67")) mustEqual (Success(Tuple2(Moz(123.45), Intensity(456.67))))
    }
    """get ("123.45 456.67",23) """ in {
      LoaderMGF.textLine2MozIntensity(Some("123.45 456.67")) mustEqual (Success(Tuple2(Moz(123.45), Intensity(456.67))))
    }

    """get ("123.45 45E+2",23) """ in {
      LoaderMGF.textLine2MozIntensity(Some("123.45 45E+2")) mustEqual (Success(Tuple2(Moz(123.45), Intensity(4500))))
    }
    """get ("123.45) """ in {
      LoaderMGF.textLine2MozIntensity(Some("123.45")) mustEqual (Success(Tuple2(Moz(123.45), Intensity(0))))
    }

    """get "123 xyz"  """ in {
      LoaderMGF.textLine2MozIntensity(Some("123 xyz")).isFailure mustEqual (true)
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

  "text2Precursor" should{
    "get it" in {
      val prec = LoaderMGF.text2Precursor(microblock).get

      prec.precursor.charge must equalTo(Charge(2))

      prec.title must equalTo("20141008_BSA_25cm_column2.10988.10988.2")

    }
  }

//  "load" should {
//    val run = LoaderMGF.load("test/resources/F001644.mgf")
//    "get idRun out of filename" in {
//      run.id must equalTo(IdRun("F001644"))
//    }
//    "count the msms" in {
//      run.msnSpectra.size must equalTo(1822)
//    }
//  }
}
