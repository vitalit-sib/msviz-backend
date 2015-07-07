package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ProteinMatchMultipleSearchesSpecs extends Specification {

  "proteinMatchMultipleSearches" should {
    def protIdentInfo(ac:String) = ProteinIdentInfo(AccessionCode(ac), SequenceSource("db"), IdentScore(1.0, Map()), 1, 2, true)
    def protIdent(searchId:String, ac:String) =
      ProteinIdent(SearchId(searchId), protIdentInfo(ac), Seq())

    val mainProt1 = ProteinIdentInfo(AccessionCode("P1"), SequenceSource("db"), IdentScore(1.0, Map()), 1, 2, true)
    val mainProt2 = ProteinIdentInfo(AccessionCode("P2"), SequenceSource("db"), IdentScore(2.0, Map()), 1, 3, true)
    val proteinInfo1 = ProteinIdent(SearchId("s1"), mainProt1, Seq())
    val proteinInfo2 = ProteinIdent(SearchId("s2"), mainProt2, Seq())
    val proteinInfo3 = ProteinIdent(SearchId("s3"), mainProt2, Seq())
    val proteinMatchMulti = ProteinMatchMultipleSearches(Map())

/*
    def toMap(pmms: ProteinMatchMultipleSearches) =
      pmms.dict.map({ case (p, pinfos) =>
        (p.value, pinfos.map(_.searchId.value).sorted)
      })
*/
    def toMap(pmms:ProteinMatchMultipleSearches) =
    pmms.dict.map({ case (p, pmap) =>
      (p.value, pmap.map({
        case(s,pinfos)=>
          (s.value,pinfos.map(_.searchId.value).sorted)
      }
      ))
    })


    "add one protein" in {
      val pmm = proteinMatchMulti.add(SearchId("s1"),proteinInfo1)
      toMap(pmm) must equalTo(Map("P1"->List("s1")))
    }
    "add twice the same proteinInfo" in {
      val pmm = proteinMatchMulti.add(SearchId("s1"),proteinInfo1).add(SearchId("s1"),proteinInfo1)
      toMap(pmm) must equalTo(Map("P1"->List("s1")))
    }

    "add proteinInfo" in {
      val proteinMatchMulti1 = proteinMatchMulti.add(SearchId("s1"),proteinInfo1)
      val proteinMatchMulti2 = proteinMatchMulti1.add(SearchId("s1"),proteinInfo1)
      val proteinMatchMulti3 = proteinMatchMulti2.add(SearchId("s2"),proteinInfo2)
      val proteinMatchMulti4 = proteinMatchMulti3.add(SearchId("s3"),proteinInfo3)
      println(s"HAHAHA ${toMap(proteinMatchMulti4)}")
      proteinMatchMulti4.dict.get(AccessionCode("P1")).get.size mustEqual (2)
      proteinMatchMulti4.dict.get(AccessionCode("P2")).get.size mustEqual (2)
    }

  }


}
