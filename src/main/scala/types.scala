package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._

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
  final case class Box[A](/* ??? */)
}

object exercise3 {
  // 1. scala.collection.List
  // 2. F[_, _]
  // 3. Option
  // 4. Int
  // 5. T[_[_], _]
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
