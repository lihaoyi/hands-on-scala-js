package simple

import akka.actor.ActorSystem
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp
import scala.concurrent.ExecutionContext.Implicits.global

object Router extends autowire.Server[String, upickle.Reader, upickle.Writer]{
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

object Server extends SimpleRoutingApp with Api{
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    startServer("localhost", port = 8080){
      get{
        pathSingleSlash{
          complete{
            HttpEntity(
              MediaTypes.`text/html`,
              Page.skeleton.render
            )
          }
        } ~
        getFromResourceDirectory("")
      } ~
      post{
        path("ajax" / Segments){ s =>
          extract(_.request.entity.asString) { e =>
            complete {
              Router.route[Api](Server)(
                autowire.Core.Request(
                  s,
                  upickle.read[Map[String, String]](e)
                )
              )
            }
          }
        }
      }
    }
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