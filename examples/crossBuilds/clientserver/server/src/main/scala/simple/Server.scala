package simple

import akka.actor.ActorSystem
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp
import upickle._
object Server extends SimpleRoutingApp{

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
        path("ajax"){
          extract(_.request.entity.asString) { e =>
            complete {
              val (dir, last) = e.splitAt(e.lastIndexOf("/") + 1)
              val files =
                Option(new java.io.File("./" + dir).listFiles())
                  .toSeq.flatten
              upickle.write(
                for{
                  f <- files
                  if f.getName.startsWith(last)
                } yield FileData(f.getName, f.length())
              )
            }
          }
        }
      }
    }
  }
}