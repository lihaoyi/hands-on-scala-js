package scalatex
import acyclic.file

object Util {

  implicit class Pipeable[T](t: T){
    def |>[V](f: T => V): V = f(t)
  }
}

