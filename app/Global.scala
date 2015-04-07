/**
 * Created by amasselo on 3/24/15.
 */

import play.api._
import play.api.mvc.{EssentialAction, EssentialFilter, RequestHeader, WithFilters}

import scala.concurrent.ExecutionContext.Implicits.global
object Global extends WithFilters(new CorsFilter) with GlobalSettings {

  override def onStart(app: Application) = {
    if (app.mode == Mode.Prod || (app.mode == Mode.Dev )) {

    }
  }
}

class CorsFilter extends EssentialFilter {
  def apply(next: EssentialAction) = new EssentialAction {
    def apply(requestHeader: RequestHeader) = {
      next(requestHeader).map { result =>
        result.withHeaders("Access-Control-Allow-Origin" -> "*",
          //          "Access-Control-Expose-Headers" -> "WWW-Authenticate, Server-Authorization",
          "Access-Control-Allow-Methods" -> "POST, GET, OPTIONS, PUT, DELETE"
        )
        //          "Access-Control-Allow-Headers" -> "x-requested-with,content-type,Cache-Control,Pragma,Date")
      }
    }
  }
}
