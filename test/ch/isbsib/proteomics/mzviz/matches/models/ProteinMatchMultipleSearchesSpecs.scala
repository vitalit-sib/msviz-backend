package ch.isbsib.proteomics.mzviz.matches.models

import java.io.File

import ch.isbsib.proteomics.mzviz.experimental.RunId
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.importer.LoaderMzIdent
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import org.specs2.mutable.Specification
import play.api.test.Helpers._


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ProteinMatchMultipleSearchesSpecs extends Specification {

  "proteinMatchMultipleSearches" should {
    def protIdentInfo(ac:String) = ProteinIdentInfo(AccessionCode(ac), SequenceSource("db"), IdentScore(1.0, Map()), 1, 2, true)
    def protIdent(searchId:String, ac:String) =
      ProteinIdent(SearchId(searchId), protIdentInfo(ac), Seq())

    val mainProt1 = protIdentInfo("P1")
    val mainProt2 = protIdentInfo("P2")
    val proteinInfo1 = protIdent("s1","P1")
    val proteinInfo2 = protIdent("s2","P2")
    val proteinInfo3 = protIdent("s3","P2")
    val proteinMatchMulti = ProteinMatchMultipleSearches(Map())

    def toMap(pmms:ProteinMatchMultipleSearches) =
    pmms.dict.map({ case (p, pmap) =>
      (p.value, pmap.map({
        case(s,pinfos)=>
          (s.value,pinfos.searchId.value)
      }
      ))
    })


    "add one protein" in {
      val pmm = proteinMatchMulti.add(SearchId("s1"),proteinInfo1)
      toMap(pmm) must equalTo(Map("P1" -> Map("s1" -> "s1")))
    }
    "add twice the same proteinInfo" in {
      val pmm = proteinMatchMulti.add(SearchId("s1"),proteinInfo1).add(SearchId("s1"),proteinInfo1)
      toMap(pmm) must equalTo(Map("P1" -> Map("s1" -> "s1")))
    }

    "add several proteinInfo" in {
      val proteinMatchMulti1 = proteinMatchMulti.add(SearchId("s1"),proteinInfo1)
      val proteinMatchMulti2 = proteinMatchMulti1.add(SearchId("s1"),proteinInfo1)
      val proteinMatchMulti3 = proteinMatchMulti2.add(SearchId("s2"),proteinInfo2)
      val proteinMatchMulti4 = proteinMatchMulti3.add(SearchId("s3"),proteinInfo3)
      toMap(proteinMatchMulti4) must equalTo(Map("P1" -> Map("s1" -> "s1"), "P2" -> Map("s2" -> "s2","s3" -> "s3")))
      proteinMatchMulti4.dict.get(AccessionCode("P1")).get.size mustEqual (1)
      proteinMatchMulti4.dict.get(AccessionCode("P2")).get.size mustEqual (2)
    }

  }

  "proteinMatchMultipleSearches from files" should {
      val psmAndProtLists1: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/M_100.mzid"), SearchId("M_100"), RunId("M_100"), None)
      val psmAndProtLists4: Tuple3[Seq[PepSpectraMatch], Seq[ProteinIdent], SearchInfo] = LoaderMzIdent.parse(new File("test/resources/mascot/F002687_acetylation.mzid"), SearchId("F002687"), RunId("F002687"), None)

      "add proteinInfos M_100" in {
        val proteinInfo_M_100 = psmAndProtLists1._2.foldLeft(ProteinMatchMultipleSearches(Map()))((r, c) => r.add(psmAndProtLists1._3.searchId, c))
        proteinInfo_M_100.dict.size mustEqual psmAndProtLists1._2.size
      }

      "add proteinInfos M_100 twice" in {
        val proteinInfo_M_100 = psmAndProtLists1._2.foldLeft(ProteinMatchMultipleSearches(Map()))((r, c) => r.add(psmAndProtLists1._3.searchId, c))
        val proteinInfo_M_100_2 = psmAndProtLists1._2.foldLeft(proteinInfo_M_100)((r, c) => r.add(psmAndProtLists1._3.searchId, c))
        proteinInfo_M_100_2.dict.size mustEqual psmAndProtLists1._2.size
      }

      "add proteinInfos M_100 and F002687" in {
        val proteinInfo_1 = psmAndProtLists1._2.foldLeft(ProteinMatchMultipleSearches(Map()))((r, c) => r.add(psmAndProtLists1._3.searchId, c))
        val proteinInfo_2 = psmAndProtLists4._2.foldLeft(proteinInfo_1)((r, c) => r.add(psmAndProtLists4._3.searchId, c))

        val albu_human = proteinInfo_2.dict.filter(_._2.size > 1)
        albu_human.get(AccessionCode("ALBU_HUMAN")).get.size mustEqual (2)

        // ALBU_HUMAN should be in both runs
        proteinInfo_2.dict.size mustEqual (psmAndProtLists1._2.size + psmAndProtLists4._2.size - 1)

      }

  }


}
