package ch.isbsib.proteomics.mzviz.matches.importer

import java.io.File

import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.maxquant.ProteinGroupsTable
import org.specs2.mutable.Specification


/**
 * @author Roman Mylonas & Trinidad Martin
 * copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
class LoaderMaxQuantSpecs extends Specification{

  "parse prot group" in {

    val listProteinGroups= LoaderMaxQuant.loadProtMaxQuant(new File("test/resources/maxquant/proteinGroups.txt"),SearchId("wewe"))
     // Obtain info for first runId F002453
    val firstRunIdInfo : List[ProteinGroupsTable]=listProteinGroups(0)
    firstRunIdInfo.size mustEqual(139)
    val secondRunIdInfo : List[ProteinGroupsTable]=listProteinGroups(1)
    secondRunIdInfo.size mustEqual(139)

    //2 runids
    listProteinGroups.size mustEqual(2)

  }

}
