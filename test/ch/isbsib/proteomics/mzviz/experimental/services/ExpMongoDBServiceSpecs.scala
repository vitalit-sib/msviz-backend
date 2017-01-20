package ch.isbsib.proteomics.mzviz.experimental.services

import java.io.File

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.{MSRun, RunId, SpectrumUniqueId}
import ch.isbsib.proteomics.mzviz.experimental.importer._
import ch.isbsib.proteomics.mzviz.experimental.models.{ExpPeakMSn, SpectrumId}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */

class ExpMongoDBServiceSpecs extends Specification with ScalaFutures with TempMongoDBForSpecs {
  implicit val defaultPatience =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  // create the default service
  val service = new ExpMongoDBService(db)

  sequential

  "empty service" should {
    "counts are 0" in {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)
    }
  }

  "create 2 runs" should {
    "get them up " in {
      service.countMsnSpectra.futureValue must equalTo(0)
      service.countMsRuns.futureValue must equalTo(0)

      val msnRun1= new MSRun(RunId("test-1"),LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-1")).toSeq)
      val msnRun2= new MSRun(RunId("test-2"),LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-2")).toSeq)
      service.insert(msnRun1).futureValue
      service.insert(msnRun2).futureValue

      service.countMsnSpectra.futureValue must equalTo(246)
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(RunId("test-1"), RunId("test-2")))
    }
  }

  "delete" should {
    "get 2 , remove 1 " in {
      service.countMsRuns.futureValue must equalTo(2)
      service.listMsRunIds.futureValue must equalTo(List(RunId("test-1"), RunId("test-2")))
      service.delete(Set(RunId("test-1"))).futureValue
      service.countMsRuns.futureValue must equalTo(1)
      service.listMsRunIds.futureValue must equalTo(List(RunId("test-2")))

    }
  }

  "findSpectrumByRunIdwithEmptySpectra" should {
    "find eight" in {

      val msnRun1= new MSRun(RunId("test-empty"),LoaderMGF.load(new File("test/resources/mascot/F003077.mgf"), RunId("test-empty")).toSeq)

      val n= service.insert(msnRun1).futureValue
      val sp = service.findSpectrumByRunId(RunId("test-empty")).futureValue.toList
      sp.length must equalTo(21)

      val spFiltered=sp.filter(_.peaks.size == 0)
      spFiltered.length must equalTo(8)

    }
  }


  "findSpectrumByRunIdAndTitle" should {
    "find one" in {
      val msnRun1= new MSRun(RunId("test-1"),LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("test-1")).toSeq)
      val n= service.insert(msnRun1).futureValue

      val sp = service.findSpectrumByRunIdAndTitle(RunId("test-1"), "File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 56.254 min, Period: 1, Cycle(s): 2083 (Experiment 4)").futureValue
      sp.ref.spectrumId.runId must equalTo(RunId("test-1"))
      sp.ref.title must equalTo("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 56.254 min, Period: 1, Cycle(s): 2083 (Experiment 4)")

      sp.peaks must have size 190

      val p2 = sp.peaks(2)
      p2 must equalTo(ExpPeakMSn(Moz(86.0752), Intensity(0.0083), IntensityRank(63), MSLevel(2)))

    }
  }

  "findSpectrumByRunId" should {
    "find one" in {
      val sp = service.findSpectrumByRunId(RunId("test-1")).futureValue.toList
      println(sp.last.ref.spectrumId.id.value)

      sp.length must equalTo(123)

    }
  }


  "findSpectrumBySpId" should {
    "find one" in {
      val spId = new SpectrumId(SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 52.948 min, Period: 1, Cycle(s): 2056 (Experiment 3)"), RunId("test-1"))
      val sp = service.findSpectrumBySpId(spId).futureValue

      sp.ref.title mustEqual("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 52.948 min, Period: 1, Cycle(s): 2056 (Experiment 3)")

    }
  }


  "findAllSpectraRefByrunId" should {
    "find all with one runID" in {
      val spRefs = service.findAllSpectraRefByrunId(RunId("test-1")).futureValue.toList
      spRefs must have size (123)
      spRefs(0).spectrumId.runId must equalTo(RunId("test-1"))

    }

    "find all with one runId in set" in {

      val msnRun1= new MSRun(RunId("test-1"),LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("chanclas_0")).toSeq)
      val msnRun2= new MSRun(RunId("test-2"),LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("chanclas_1")).toSeq)
      val msnRun3= new MSRun(RunId("test-2"),LoaderMGF.load(new File("test/resources/mascot/M_100.mgf"), RunId("chanclas_2")).toSeq)

      service.insert(msnRun1).futureValue
      service.insert(msnRun2).futureValue
      service.insert(msnRun3).futureValue

      val spRefs = service.findAllSpectraRefByrunId(Set(RunId("chanclas_0"), RunId("chanclas_2"))).futureValue.toList
      spRefs must have size (123*2)
      spRefs(0).spectrumId.runId must equalTo(RunId("chanclas_0"))

    }

  }

  "findSpectrumByMozTol" should {
    "find one" in {
      val spList = service.findSpectrumByMozTol(RunId("test-1"), Moz(406), 0.5).futureValue
      spList.length mustEqual(3)
    }

    "find one without peaks" in {
      val spRefList = service.findSpectrumRefByMozTol(RunId("test-1"), Moz(406), 0.5).futureValue

      spRefList.length mustEqual(3)
    }
  }

  "findSpectrumByMassTol" should {
    "find one" in {
      val spList = service.findSpectrumRefByMassTol(RunId("test-1"), Moz(406), Charge(2), 1300).futureValue
      spList.length mustEqual(2)
    }
  }


  "find sp and sp-ref by runId and scanNr" should {
    val spId = new SpectrumId(SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 52.948 min, Period: 1, Cycle(s): 2056 (Experiment 3)"), RunId("test-1"))

    "find sp" in {
      val sp = service.findSpectrumByRunIdAndScanNumber(RunId("test-1"), spId.id).futureValue
      sp.peaks.size mustEqual(131)
      sp.ref.precursor.moz mustEqual(Moz(441.759509))
    }

    "find sp-ref" in {
      val spRef = service.findSpectrumRefByRunIdAndScanNumber(RunId("test-1"), spId.id).futureValue
      spRef.precursor.moz mustEqual(Moz(441.759509))
      spRef.spectrumId.id mustEqual(spId.id)
    }

  }

  "update molMass" should {

    val spId = new SpectrumId(SpectrumUniqueId("File: 141206_QS_FRB_rafts_SBCL2_complmix.wiff, Sample: 3i, complex mix method (sample number 1), Elution: 52.948 min, Period: 1, Cycle(s): 2056 (Experiment 3)"), RunId("test-1"))

    "change and check molMass" in {
      val res = service.findAndUpdateMolMass(spId, MolecularMass(999.99)).futureValue
      res mustEqual(true)

      val spRef = service.findSpectrumRefByRunIdAndScanNumber(RunId("test-1"), spId.id).futureValue
      spRef.precursor.molecularMass.get.value mustEqual(999.99)
    }

  }



}

