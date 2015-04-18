package ch.isbsib.proteomics.mzviz.commons.helpers

import com.google.common.base.Optional

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
object OptionConverter {

  /**
   * converts a com.google.common.base.Optional to a SearchInfoMongoDBServiceXXX.scala Option
   * @param option com.google.common.base.Optional of any type T
   * @return SearchInfoMongoDBServiceXXX.scala option of same type T
   */
   implicit def convertGoogleOption[T](option: Optional[T]): Option[T] = {
    def convert(option: Optional[T]) = option.isPresent() match {
      case true => Some(option.get())
      case false => None
    }
    convert(option)
  }

}
