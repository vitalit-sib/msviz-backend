package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class ProteinMatchMultipleSearchesSpecs extends Specification {

  "proteinMatchMultipleSearches" should {
    val mainProt1= ProteinIdentInfo(AccessionCode("P1"), SequenceSource("db"), IdentScore(1.0, Map()), 1, 2, true)
    val mainProt2=ProteinIdentInfo(AccessionCode("P2"), SequenceSource("db"), IdentScore(2.0, Map()), 1, 3, true)
    val proteinInfo1= ProteinIdent(SearchId("s1"), mainProt1, Seq())
    val proteinInfo2= ProteinIdent(SearchId("s2"), mainProt2, Seq())
    val proteinInfo3= ProteinIdent(SearchId("s3"), mainProt2, Seq())
    val proteinMatchMulti=ProteinMatchMultipleSearches(Map())

    "add proteinInfo" in {
      val proteinMatchMulti1 = proteinMatchMulti.add(proteinInfo1)
      val proteinMatchMulti2 = proteinMatchMulti1.add(proteinInfo1)
      val proteinMatchMulti3 = proteinMatchMulti2.add(proteinInfo2)
      val proteinMatchMulti4 = proteinMatchMulti3.add(proteinInfo3)
      proteinMatchMulti4.dict.get(AccessionCode("P1")).get.size mustEqual(2)
      proteinMatchMulti4.dict.get(AccessionCode("P2")).get.size mustEqual(2)
    }

  }


  }
