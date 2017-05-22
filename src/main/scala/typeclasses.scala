package lambdaconf.typeclasses

import matryoshka._
import monocle._

import scalaz._
import Scalaz._
import scala.util.Try

object exercise1 {

  /** Laws
    * forAll[A](a => read(show(a)) == Some(a))
    */
  trait Debuggable[A] {
    def show(a: A): String
    def read(s: String): Option[A]
  }

  object Debuggable {
    def apply[A](implicit d: Debuggable[A]) = d

    implicit val DebuggableString = new Debuggable[String] {
      def show(a: String) = a
      def read(s: String) = Some(s)
    }

    implicit val DebuggableInt = new Debuggable[Int] {
      def show(a: Int) = a.toString
      def read(s: String) = Try(s.toInt).toOption
    }
  }

  implicit class DebuggagleSyntax[A](a: A) {
    def show(implicit d: Debuggable[A]) = d.show(a)
  }



  sealed trait PathLike[A] {
    // ???
  }
  object PathLike {
    def apply[A: PathLike]: PathLike[A] = implicitly[PathLike[A]]
  }
}

object exercise2 {
  import exercise2._
}

object exercise3 {
  import exercise1._
  import exercise2._

  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val NodePathLike: PathLike[Node] = new PathLike[Node] {
    // ???
  }
}

object exercise4 {
  import exercise1._
  import exercise2._
  import exercise3._

  implicit class PathLikeSyntax[A: PathLike](self: A) {
    // ???
  }
}
