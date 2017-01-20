package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.commons._
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumId
import ch.isbsib.proteomics.mzviz.experimental.{SpectrumUniqueId, RunId}
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.modifications.{ModifName}
import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}
import org.specs2.mutable.Specification

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class PepSpectraMatchSpecs extends Specification {
  "pepSpectraMatch" should {

    // vals used in following tests
    val score = IdentScore(mainScore = 0.8, scoreMap = Map("p-value" -> 0.001))
    val matching = PepMatchInfo(score, numMissedCleavages = Some(1), rank= Some(1), correctedMoz = Some(100.23), correctedMolMass = Some(1232.23),
      massDiff = Some(0.01), massDiffUnit = Some(PPM), totalNumIons = Some(1),
      isRejected = None, chargeState = Some(2), modificationProbabilities = None, highestModifProbability = None)
    val sequence = "AKKKAA"
    val oneModifRef = ModifName("Phospho")
    val modificationRefs = Vector(Nil, Seq(oneModifRef), Nil, Nil, Nil, Nil)
    val pep = Peptide(sequence = sequence, molMass = Some(123.23), modificationNames = modificationRefs)
    val protRef = ProteinRef(AC = AccessionCode("AC001"), source = Some(SequenceSource("dbref")))
    val protMatch = Seq(ProteinMatch(proteinRef = protRef, previousAA = Some("A"), nextAA = Some("K"), startPos = 1, endPos = 10, Some(false)))

    "create simple Peptide" in {
      pep.molMass must equalTo(Some(123.23))
    }

    "create simple PepMatchInfo" in {
      matching.score.mainScore must equalTo(0.8)
      matching.score.scoreMap("p-value") must equalTo(0.001)
      matching.isRejected should be(None)
    }

    "create simple ProteinMatch" in {
      protMatch.size mustEqual 1
      protMatch(0).proteinRef.AC mustEqual AccessionCode("AC001")
      protMatch(0).proteinRef.source mustEqual Some(SequenceSource("dbref"))
    }

    "create simple PepSpectraMatch" in {
      val pepSpMatch = PepSpectraMatch(searchId = SearchId("a search result"), spectrumId = SpectrumId(SpectrumUniqueId("1"), runId = RunId("blabla.mgf")), pep = pep, matchInfo = matching, proteinList = protMatch)
      pepSpMatch.spectrumId.id must equalTo(SpectrumUniqueId("1"))
      pepSpMatch.spectrumId.runId must equalTo(RunId("blabla.mgf"))
      pepSpMatch.matchInfo.massDiff must equalTo(Some(0.01))
      pepSpMatch.searchId must equalTo(SearchId("a search result"))
    }

  }

}
