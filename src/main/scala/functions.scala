package lambdaconf.functions

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  // Domain: {Vegetables, Fruits, Meat, Dairy, Eggs}
  // Codomain: {Love, Like, Neutral, Dislike, Hate}
}

object exercise2 {
  val compareStrings: (Char => Char) => (String, String) => Boolean = ???
}

object exercise3 {
  type Parser[A] = String => Either[String, (String, A)]
  type ErrorMsg = String
  type Input = String

  def fail[A](err: ErrorMsg): Parser[A] = (_: Input) => Left(err)

  def anyChar: Parser[Char] = (input: Input) =>
    if (input.length == 0)
      Left("expected char, found EOF")
    else
      Right((input.drop(1), input.charAt(0)))

  def satisfies[A](p: Parser[A])(f: A => Boolean): Parser[A] =
    (input: String) => p(input) match {
      case Left(err) => Left(err)
      case Right((input, a)) =>
        if (f(a)) Right((input, a))
        else Left("predicate failed")
    }

  val parseLeftCBracket: Parser[Char] = satisfies(anyChar)(_ == '{')
  val parseLeftSBracket: Parser[Char] = satisfies(anyChar)(_ == '{')

  val parseLeftAnyBracket: Parser[Char] = or(parseLeftCBracket, parseLeftSBracket)

  def or[A](left: Parser[A], right: Parser[A]): Parser[A] =
    (input: Input) => left(input) match {
      case Left(_) => right(input)
      case x => x
    }

  def then[A, B](first: Parser[A], second: Parser[B]): Parser[(A, B)] =
//    (input) => first(input) match {
//      case Right(i, a) => second(i) match {
//        case Right(t) => ???
//        case Left(e) => ???
//      }
//      case l => l
//    }
}

object exercise4 {
  def snd[A, B](v: (A, B)): B = ???
}
