package ch.isbsib.proteomics.mzviz.qc.models

import ch.isbsib.proteomics.mzviz.qc


/**
 * Created by qjolliet on 28/07/15.
 */
case class QcSummaryEntry(rawfileInfomation:RawfileInfomation,MS:Int,MMS:Int,MmsIdentify:Int,PeptideSeq:Int)

case class RawfileInfomation(proteinName:String,pQuantity:String,machineName:String,columnType:String,Date:String,Index:String)