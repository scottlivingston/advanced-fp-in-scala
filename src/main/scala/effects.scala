package lambdaconf.effects

import matryoshka._
import monocle._
import scalaz._
import scala.io.StdIn._
import Scalaz._

object exercise1 {
  println("enter a row:")
  val line = readLine()

  // NOT functions in the context of fp!
  // val println: String => ()
  // val readline: () => String

  // convert from an effect to a recipe for an effect
  // val println: String => IO[()]
  // val readLine: () => IO[String]

  object Console {
    val putStrinLn: String => IO[()] =
      line => IO(unsafePerformIO = () => println(line))
    val getStrLn: IO[String] =
      IO(unsafePerformIO = () => readLine())
  }


  final case class IO[A](unsafePerformIO: () => A)

  implicit val IOMonad: Monad[IO] = new Monad[IO] {
    override def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = IO(() => f(fa.unsafePerformIO()).unsafePerformIO())
    override def point[A](a: => A): IO[A] = IO(() => a)
  }

  import Console._
  val program: IO[String] = for {
    _ <- putStrinLn("whats your name?:")
    name <- getStrLn
    _ <- putStrinLn(s"whats up $name")
  } yield name
}

object exercise2 {
  import exercise1._

  sealed trait ConsoleF[A]

  val program: Free[ConsoleF, Unit] = ???
}

object exercise3 {
  final case class State[S, A](/* ??? */) {
    def evalState(s: S): A = ???
  }

  implicit def StateMonad[S]: Monad[State[S, ?]] = ???
}

object exercise4 {
  import exercise3._

  def sort[A: Order](list: List[A]): List[A] =
    (??? : State[List[A], List[A]]).evalState(list)
}
