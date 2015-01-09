package ch.isbsib.proteomics.mzviz.matches.models

import ch.isbsib.proteomics.mzviz.theoretical.{SequenceSource, AccessionCode}

/**
 * Created by Roman Mylonas on 19/12/14.
 */
case class ProteinRef(AC: AccessionCode, source: Option[SequenceSource])
