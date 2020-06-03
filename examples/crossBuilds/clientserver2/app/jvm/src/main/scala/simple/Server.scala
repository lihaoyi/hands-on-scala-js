package simple

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Properties

import upickle.default._

object Router extends autowire.Server[String, Reader, Writer]{
  def read[Result: Reader](p: String): Result = upickle.default.read[Result](p)
  def write[Result: Writer](r: Result): String = upickle.default.write(r)
}

object Server extends Api{
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()

    val port = Properties.envOrElse("PORT", "8080").toInt
    val route = {
      get{
        pathSingleSlash{
          complete{
            HttpEntity(
              ContentTypes.`text/html(UTF-8)`,
              Page.skeleton.render
            )
          }
        } ~
          getFromResourceDirectory("")
      } ~
        post{
          path("ajax" / Segments){s =>
            entity(as[String]) { e =>
              complete {
                Router.route[Api](Server)(
                  autowire.Core.Request(
                    s,
                    upickle.default.read[Map[String, String]](e)
                  )
                )
              }
            }
          }
        }
    }
    Http().bindAndHandle(route, "0.0.0.0", port = port)
  }
  def list(path: String) = {
    val (dir, last) = path.splitAt(path.lastIndexOf("/") + 1)
    val files =
      Option(new java.io.File("./" + dir).listFiles())
        .toSeq.flatten
    for{
      f <- files
      if f.getName.startsWith(last)
    } yield FileData(f.getName, f.length())
  }
}
