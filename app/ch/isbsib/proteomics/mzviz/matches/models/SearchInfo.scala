package ch.isbsib.proteomics.mzviz.matches.models

import java.text.SimpleDateFormat

import ch.isbsib.proteomics.mzviz.matches.SearchId

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class SearchInfo (searchId: SearchId,
                       title: Option[String],
                       date:SimpleDateFormat, jobNumber:Int,
                       database:String,
                       username:Option[String])
