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
  def snd[A](t: (_, A)): A = t._2

  trait Foo {
    type A //TYPE MEMBER

    def newA: A
  }

  def acceptFoo(v: Foo) = {
    val f: v.A = v.newA

  }

  trait SmartList[A] { self =>
    type A0 //<: can bound this type, becomes a bound existential type

    protected def mapper: A0 => A
    protected def list: List[A0]

    def run: List[A] = list.map(mapper)
    def map[B](f: A => B): SmartList[B] = new SmartList[B] {
      type A0 = self.A0
      protected def mapper = self.mapper.andThen(f)
      protected def list = self.list
    }
  }

  object SmartList {
    def apply[A](l: List[A]): SmartList[A] = new SmartList[A] {
      type A0 = A

      protected def mapper = a => a
      protected def list = l
    }
  }

  trait Stack {
    type Stack[_]

    def newStack[A]: Stack[A]

    def push[A](v: A, s: Stack[A]): Stack[A]
    def pop[A](s: Stack[A]): Option[(A, Stack[A])]
  }

  def myAlgo(s: Stack) = ??? //any stack can go here cause this code knows nothing about the stack itself

  trait FileSystem {
    // ???
  }
}

object exercise5 {
  // partial application for values
  val pow: (Int, Int) => Int = ???
  /* or */
  val pow2: Int => Int => Int = ???

  val square: Int => Int = pow(_, 2)
  /* or */
  val square2: Int => Int = x => pow(x, 2)

  trait Container[F[_]] {
    def take[A](n: Int, c: F[A]): F[A]
  }


//  new Container[({type Apply[A] = Map[String, A]})#Apply]
  /* or */
//  type MapString[A] = Map[String, A]
//  new Container[MapString]
  /* or, with kind projector */
//  new Container[Map[String, ?]]


  sealed trait Example[F[_]] {
    def value: F[String]
  }

  val exampleOption = new Example[Option] {
    def value: Option[String] = Some("foo")
  }

  new Example[Either[?, Int]] { // <-- ???
    def value: Either[String, Int] = Right(2)
  }

}
