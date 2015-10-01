package ch.isbsib.proteomics.mzviz.controllers.matches

import javax.ws.rs.PathParam

import ch.isbsib.proteomics.mzviz.controllers.matches.PSMController._
import ch.isbsib.proteomics.mzviz.controllers.matches.ProteinMatchController._
import ch.isbsib.proteomics.mzviz.matches.SearchId
import ch.isbsib.proteomics.mzviz.matches.models.ProteinMatchMultipleSearches
import ch.isbsib.proteomics.mzviz.matches.services.{MatchMongoDBService, ProteinMatchMongoDBService}
import ch.isbsib.proteomics.mzviz.theoretical.AccessionCode
import com.wordnik.swagger.annotations._
import play.api.cache.Cached
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import ch.isbsib.proteomics.mzviz.matches.services.JsonMatchFormats._
import play.api.Logger


/**
 * @author Roman Mylonas, Trinidad Martin & Alexandre Masselot
 *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
 */
@Api(value = "/proteinListMultipleSearches", description = "list of identified proteins for all given searches")
object ProteinMatchMultipleSearchesController extends MatchController {


  @ApiOperation(nickname = "findAllProteinsForMultipleSearchIds",
    value = "find all proteins corresponding to given searchIds",
    notes = """protein list""",
    response = classOf[List[String]],
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "withModif", value = "modification name", required = false, dataType = "String", paramType = "query")
  ))
  def findAllProteinsForMultipleSearchIds(
                                           @ApiParam(value = """searchIds""") @PathParam("searchIds") searchIds: String,
                                           withModif: Option[String]
//Fast enough without cache, and it keeps the erros   ) = Cached(req => req.uri) {
                                           )={
    Action.async {
      val sids = queryParamSearchIds(searchIds)

      val proteinMultiList = ProteinMatchMongoDBService().findAllProteinsBySearchIds(sids).map(
          _.foldLeft( ProteinMatchMultipleSearches(Map()) )( (r, c) => r.add(c.searchId, c) )
      )

      val validACs = MatchMongoDBService().listProteinRefsBySearchIds(queryParamSearchIds(searchIds), queryParamOModifName(withModif)).map(_.map(_.AC.value))

      // we filter on selected modif in case there is one selected
      val filteredMultiList = withModif match {

        case Some(name) => {
          // get the list of valid AC's
          val validACs = MatchMongoDBService().listProteinRefsBySearchIds(queryParamSearchIds(searchIds), queryParamOModifName(withModif)).map(protRef => protRef.map(_.AC.value))
          val filteredProts = validACs.flatMap({ acs =>
            // get the intersection of ACs
            val filterdFutureProts = proteinMultiList.map({ prot =>
              val protACs = prot.dict.filter({case(k,v) => acs.contains(k.value)})
              ProteinMatchMultipleSearches(protACs)
            })
            filterdFutureProts
          })
          filteredProts
        }

        case None => proteinMultiList
      }

      for{
        unFuturedList <- filteredMultiList
      }yield{
        Ok(Json.toJson(unFuturedList))
      }

    }
  }

}

