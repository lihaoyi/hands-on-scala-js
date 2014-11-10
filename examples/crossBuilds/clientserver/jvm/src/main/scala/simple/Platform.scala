//jvm/src/main/scala/simple/Platform.scala
package simple

import java.text.SimpleDateFormat

import akka.actor.ActorSystem
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp

object Platform extends SimpleRoutingApp{
  def format(ts: Long) = {
    val fmt =
      "MMMM d, yyyy h:mm:ss aaa z"
    new SimpleDateFormat(fmt).format(
      new java.util.Date(ts)
    )
  }

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    startServer("localhost", port = 8080){
      get{
        pathSingleSlash{
          complete{
            val msg = Simple.formatTimes(Seq(0, 1 << 30))
            val boot =
              "Platform().main(document.getElementById('contents'))"
            HttpEntity(
              MediaTypes.`text/html`,
              s"""
                <html>
                  <head>
                    <script src="/js-fastopt.js"></script>
                  </head>
                  <body onload="$boot">
                    <div id="contents">
                      <h1>Hello from Scala-JVM!</h1>
                      <p>$msg</p>
                    </div>
                  </body>
                </html>
              """
            )
          }
        } ~
        getFromResourceDirectory("")
      } ~
      post{
        path("formatDates"){
          extract(_.request.entity.asString) { e =>
            complete {
              Simple.formatTimes(e.split(",").map(_.toLong))
            }
          }
        }
      }
    }
  }
}