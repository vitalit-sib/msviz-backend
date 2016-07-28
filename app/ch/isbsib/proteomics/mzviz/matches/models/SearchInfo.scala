package ch.isbsib.proteomics.mzviz.matches.models

import java.text.SimpleDateFormat

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.theoretical.models.SearchDatabase
import java.util.Date

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class SearchInfo (searchId: SearchId,
                       title: String,
                       database: Seq[SearchDatabase],
                       username:String,
                       enzyme: String,
                       parentTolerance: Option[String],
                       fragmentTolerance: String,
                       status:String,
                       creationDate: Option[Date]
                        )
