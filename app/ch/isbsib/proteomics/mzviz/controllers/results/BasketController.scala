//package ch.isbsib.proteomics.mzviz.controllers.results
//
//import ch.isbsib.proteomics.mzviz.controllers.matches.SearchController._
//import ch.isbsib.proteomics.mzviz.matches.services.SearchInfoDBService
//import com.wordnik.swagger.annotations.{Api, ApiOperation}
//import play.api.libs.json.Json
//import play.api.mvc.Action
//import scala.concurrent.Future
//
///**
// * @author Roman Mylonas & Trinidad Martin
// *         copyright 2014-2015, SIB Swiss Institute of Bioinformatics
// */
//@Api(value = "/basket", description = "basket")
//object BasketController {
//
//  @ApiOperation(nickname = "put",
//    value = "put basket entry",
//    notes = """put a new basket entry or update an existing one""",
//    response = classOf[Boolean],
//    httpMethod = "PUT")
//  def put =
//    Action.async {
//      Future  {Ok("ok")}
////      SearchInfoDBService().list.map {
////        searchInfos => Ok(Json.toJson(searchInfos))
////    }
//    }
//
//
//
//}
