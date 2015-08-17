package ch.isbsib.proteomics.mzviz


import java.util

import sun.util.calendar.LocalGregorianCalendar.Date

/**
 * Created by qjolliet on 07/08/15.
 */
package object qc {
  //proteinName
  case class ProteinName(value: String) extends AnyVal {
    override def toString = value
  }
  case class ProteinQuantity(value: String) extends AnyVal {
    override def toString = value
  }
  case class MachineName(value: String) extends AnyVal {
    override def toString = value
  }
  case class ColumnType(value: String) extends AnyVal {
    override def toString = value
  }
  //case class QcDate(value: String) extends AnyVal {
  case class QcDate(value: util.Date) extends AnyVal

  case class QcIndex(value: String) extends AnyVal {
    override def toString = value
  }
}
