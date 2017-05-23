package lambdaconf.patterns

import matryoshka._
import monocle._

import scalaz._
import Scalaz._
import scala.concurrent.Future
import scala.util.Try
import scala.language.higherKinds

object exercise1 {
  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def readRowCol(): Option[(Int, Int)] = {
    println("Please enter a row:")
    val row = readLine()
    println("Please enter a column:")
    val col = readLine()
    for {
      r <- parseInt(row)
      c <- parseInt(col)
    } yield (r, c)
  }

  sealed trait FileSystemError
  final case class FileNotFound(path: String) extends FileSystemError
  final case class FileLocked(path: String, expires: Int) extends FileSystemError
  final case class FilePermission(path: String, group: String) extends FileSystemError
  def readFile(path: String): FileSystemError \/ Array[Byte] = ???
}

object exercise2 {
  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val NodeMonoid: Monoid[Node] = ???
}

object exercise3 {

  def head: List ~> Option = new ~>[List, Option] {
    def apply[A](v: List[A]): Option[A] = v.headOption
  }


  case class Eager[A](run: A) // Functor! Strict Identity Functor, Eager
  case class Deferred[A](run: () => A) // Functor! Deferred
  case class Memoized[A](private val run0: () => A) {
    lazy val run = run0()
  } // Functor! Memoized Lazily

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit def FunctionFunctor[A0]: Functor[Function[A0, ?]] = new Functor[Function[A0, ?]] {
    override def map[A, B](fa: Function[A0, A])(f: A => B): Function[A0, B] =
      a0 => f(fa(a0))
  }


  trait Apply[F[_]] extends Functor[F] {
    def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]
  }

  trait ApplyZip[F[_]] {
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  implicit def OptionApply: Apply[Option] = new Apply[Option] {
    override def ap[A, B](fa: Option[A])(fab: Option[(A) => B]): Option[B] = ???
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = ???
  }



  trait Bind[F[_]] extends Apply[F] {
    def bind[A, B](fa: F[A])(afb: A => F[B]): F[B]
  }

  implicit val BindOption: Bind[Option] = new Bind[Option] {
    override def bind[A, B](fa: Option[A])(afb: (A) => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => afb(a)
      }

    override def ap[A, B](fa: Option[A])(fab: Option[(A) => B]): Option[B] = ???
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = ???
  }


  sealed trait Parser[A]
  final case class ParseChar() extends Parser[Char]
  final case class ParseChars(int: Int) extends Parser[String]
  final case class ParseApply[A0](f: Parser[A0 => A], a: Parser[A]) extends Parser[A]
  final case class ParsePoint[A](value: A) extends Parser[A]

  implicit val ApplyParser = new Apply[Parser] {
    def ap[A, B](fa: Parser[A])(fab: Parser[A => B]) = ParseApply(fab, fa)
  }

  val OptionToFuture: Option ~> Future = new ~>[Option, Future] {
    override def apply[A](fa: Option[A]): Future[A] = fa match {
      case None => Future.failed(new Exception("awe shit"))
      case Some(o) => Future(o)
    }
  }

  final case class Thunk[A](run: () => A)

  implicit val MonadThunk: Monad[Thunk] = ???
}

object exercise4 {
  def filter[A](f: A => Boolean, l: List[A]): List[A] = {
    val foldable = Foldable[List]

    ???
  }
}

object exercise5 {
  trait List[A] { self =>
    def fold[Z](nil: => Z, cons: (Z, A) => Z): Z

    final def :: (next: A): List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = {
        cons(self.fold(nil, cons), next)
      }
    }
  }
  object List {
    def empty[A]: List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = nil
    }
  }

  implicit val ListTraverse: Traverse[List] = ???
}
