package simple

import akka.actor.Actor
import spray.http.{HttpEntity, MediaTypes}
import spray.routing._
import scala.concurrent.ExecutionContext.Implicits.global

object Router extends autowire.Server[String, upickle.Reader, upickle.Writer]{
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService with Api {
  self: MyService =>
  val myRoute =
    get {
      pathSingleSlash {
        complete {
          HttpEntity(
            MediaTypes.`text/html`,
            Page.skeleton.render
          )
        }
      } ~
      getFromResourceDirectory("")
    } ~
      post {
        path("atmosphere/ajax" / Segments) { s =>
          extract(_.request.entity.asString) { e =>
            complete {
              Router.route[Api](self)(
                autowire.Core.Request(s, upickle.read[Map[String, String]](e))
              )
            }
          }
        }
      }

  def list(path: String): Seq[FileData] = {
    val (dir, last) = path.splitAt(path.lastIndexOf("/") + 1)
    val files =
      Option(new java.io.File("./" + dir).listFiles())
        .toSeq.flatten
    for{
      f <- files
      if f.getName.startsWith(last)
    }
    yield FileData(f.getName, f.length())
  }
}