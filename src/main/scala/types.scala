package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._
import scala.language.higherKinds
import Scalaz._

object exercise1 {
  final case class Line[A](p1: A, p2: A, p3: A, p4: A)
  final case class CheckersBoard(value: Line[Line[Option[CheckerPiece]]])

  sealed trait CheckerPiece
  case object RedPiece          extends CheckerPiece
  case object BlackPiece        extends CheckerPiece
  case object CrownedRedPiece   extends CheckerPiece
  case object CrownedBlackPiece extends CheckerPiece
}

object exercise2 {
  final case class Box[A](value: A)

  val fb: Box[Int] = ???
  val fb = Box(1)


  val f: List[Int] = ???
  // List : (X: Type) => Type
  // Int : *
  // List : * => *
  // Either : [*, *] => *
  // Map : [*, *] => *

  // val f: Either = ???  // ERROR!
  // val f: Either[String] = ???  // ERROR!
  // val f: Either[,Int] = ???  // ERROR!
  val f2: Either[String, Int] = ??? // OK!

  def identity[A](x: A): A = x // A has kind * and x has type A

  //def foo[E: [*, *] => *, A: *, B: *](value: E[A, B]) = ??? // i want to feed either to this but i can cause scala doesnt like it :(
  def foo[E[_, _], A, B](value: E[A, B]) = ??? //so i do this instead!

}

object exercise3 {
  // 1. scala.collection.List // * => *
  // 2. F[_, _] // [*, *] => *
  // 3. Option // * => *
  // 4. Int // *
  // 5. T[_[_], _] // [* => *, *] => *
                   // or
                   // (* => *) => * => *
}

object exercise4 {
  trait FileSystem {
    // ???
  }
}

object exercise5 {
  sealed trait Example[F[_]] {
    def value: F[String]
  }

  /*
  new Example[_] { // <-- ???
    def value: Either[String, Int] = Right(2)
  }
  */
}
