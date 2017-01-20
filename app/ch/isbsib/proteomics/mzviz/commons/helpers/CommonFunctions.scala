package ch.isbsib.proteomics.mzviz.commons.helpers

import ch.isbsib.proteomics.mzviz.commons.{Charge, MolecularMass, Moz}
import ch.isbsib.proteomics.mzviz.experimental.models.SpectrumRef

/**
  * @author Roman Mylonas
  *         copyright 2016-2017, SIB Swiss Institute of Bioinformatics
  */
object CommonFunctions {

  def computeMass(moz:Moz, charge:Charge):MolecularMass = MolecularMass((moz.value * charge.value) - (charge.value * 1.00728))

  def spIsValid(sp:SpectrumRef, lowerLimit:Double, upperLimit: Double):Boolean = {
    val spMass = computeMass(sp.precursor.moz, sp.precursor.charge).value
    (spMass >= lowerLimit && spMass <= upperLimit)
  }

}
