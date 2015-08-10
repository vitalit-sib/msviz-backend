package ch.isbsib.proteomics.mzviz.qc.models

import ch.isbsib.proteomics.mzviz.qc
import ch.isbsib.proteomics.mzviz.qc._



/**
 * Created by qjolliet on 28/07/15.
 */
case class QcSummaryEntry(rawfileInfomation:RawfileInfomation,MS:Int,MMS:Int,MmsIdentify:Int,PeptideSeq:Int)

case class RawfileInfomation(proteinName:ProteinName,pQuantity:ProteinQuantity,machineName:MachineName,columnType:ColumnType,Date:QcDate,Index:QcIndex)