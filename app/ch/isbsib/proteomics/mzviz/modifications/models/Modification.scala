package ch.isbsib.proteomics.mzviz.modifications.models

/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
case class Modification (
                        modifRef: ModificationRef,
                        name: String,
                        monoDeltaMass: Double
                          )
