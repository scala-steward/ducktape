package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.FallibleTransformations

case class Person(int: Int, opt: Option[Int], list: List[Int], normal: Int)
case class Person2(int: RefinedInt, opt: Option[RefinedInt], list: Vector[RefinedInt], normal: Int)

case class RefinedInt(value: Int)

enum SourceEnum {
  case PersonCase(p: Person)
}

enum DestEnum {
  case PersonCase(p: Person2)
}

object RefinedInt {
  given transformer: Transformer.Fallible[[A] =>> Either[List[String], A], Int, RefinedInt] with {
    def transform(source: Int): Either[List[String], RefinedInt] = 
      if (source == 0) Left("dupal" :: Nil) else Right(RefinedInt(source))
  }
}

object Playground extends App {
  val p = Person(1, None, List(1, 2, 3, 1), 2)
  val srcEnum = SourceEnum.PersonCase(p)
  given mode: Mode.FailFast.Either[List[String]] with {}

  val res =
    internal.CodePrinter.code:
      FallibleTransformations.between[[a] =>> Either[List[String], a], SourceEnum, DestEnum](srcEnum, mode)

  println(res)

//   println(res)
}
