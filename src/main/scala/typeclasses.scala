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


  /** Laws
    * forAll[A](a => append(zero, a) == a)
    * forAll[A](a => append(a, zero) == a)
    * forAll[L, A](a => parent(descent1(l, a)) == Some((l, a)))
    */
  trait PathLike[L, A] {
    def zero: A
    def append(a1: A, a2: A): A
    def parent(path: A): Option[(A, L)]
    def descend1(label: L, path: A): A
  }



  object PathLike {
    def apply[L, A](implicit p: PathLike[L, A]): PathLike[L, A] = implicitly[PathLike[L, A]]
  }
}

object exercise2 {
  import exercise1._
}

object exercise3 {
  import exercise1._
  import exercise2._

  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val NodePathLike: PathLike[String, Node] = new PathLike[String, Node] {
    override def zero: Node = Root
    override def append(a1: Node, a2: Node): Node = (a1, a2) match {
      case (a1, Root) => a1
      case (a1, Child(p, l)) => Child(append(a1, p), l)
    }
    override def parent(path: Node): Option[(Node, String)] = path match {
      case Root => None
      case Child(p, n) => Some((p, n))
    }
    override def descend1(label: String, path: Node): Node = Child(path, label)
  }
}

object exercise4 {
  import exercise1._
  import exercise2._
  import exercise3._

  implicit class PathLikeSyntax[L, A](self: A) {
    def parent(implicit p: PathLike[L, A]): Option[(Node, String)] = p.parent
    def / (l: L)(implicit p: PathLike[L, A]): A = p.descend1(l, self)
  }

  // path / "foo" / "bar"
  // path.parent
}
