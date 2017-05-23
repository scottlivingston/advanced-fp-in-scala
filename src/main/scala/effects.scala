package lambdaconf.effects

import matryoshka._
import monocle._
import scalaz._
import scala.io.StdIn._
import Scalaz._
import scala.language.higherKinds

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


  final case class State[S, A](run: S => (S, A))
  implicit def StateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    override def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      State(s => {
        val (s2, a) = fa.run(s)
        f(a).run(s2)
      })
    override def point[A](a: => A): State[S, A] = State(s => (s, a))

  }

  object State {
    def gets[S]: State[S, S] = State(s => (s, s))
    def puts[S](s2: S): State[S, Unit] = State(s => (s2, ()))
    def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
  }

  import State._
  val program2: State[Int, Int] = for {
    i <- gets[Int]
    _ <- puts(i + 1)
    _ <- modify[Int](_ + 2)
    i <- gets[Int]
  } yield i

  program2.run(10)


  trait Contravariant[F[_]] extends InvariantFunctor[F] {
    def contramap[A, B](r: F[A])(f: B => A): F[B]
  }

  implicit def FunctorFunction[B0] = new Contravariant[Function[?, B0]] {
    // def map[A, B](fa: Function[A, B0])(ab: A => B): B => B0 // this makes no sense and cannot be implemented
    def contramap[A, B](r: A => B0)(f: B => A): B => B0 = r.compose(f)

    override def xmap[A, B](ma: Function[A, B0], f: (A) => B, g: (B) => A): Function[B, B0] = ???
  }

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
