package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.*
import io.github.arainko.ducktape.internal.macros.*
import scala.runtime.TupleMirror

final case class Person(name: String, age: Int, additional: Int, int: Int)

final case class PersonRefined(age: newtypes.Age)


object newtypes {
  opaque type Name = String

  object Name {
    given refineName: PartialTransformer.FailFast[Option, String, Name] = Some(_)
    given refineName2: PartialTransformer.Accumulating[[A] =>> Either[::[String], A], String, Name] = 
      a => Left(::("WOOPS", Nil))
  }

  opaque type Age = Int

  object Age {
    given refineAge: PartialTransformer.FailFast[Option, Int, Age] = _ => None
    given refineAge2: PartialTransformer.Accumulating[[A] =>> Either[::[String], A], Int, Age] = a => Left(::("WOOPS2", Nil))
  }
}

object BetterTuple2Extractor {
  def unapply[A, B](tuple: (A, B)): (A, B) = tuple
}


object Playground extends App {

  // val cos = 
  //   Transformer.Debug.showCode(
  //   PartialProductTransformations.usage(Some(1), None, Some(2), Some(3))
  //   )

  // val cos2 = 
  //   DebugMacros.structure(
  //   Some(1).zip(Some(2)).zip(Some(3)).map { case ((one, two), three) => List(one, two, three) }
  //   )

  // val cos3 = DebugMacros.code(PartialProductTransformations.unnestUsage(1))

  // val personOpt = PersonOpt(Some("name"), Some(1))
  val person = Person("name", 1, 1, 1)

  val support = PartialTransformer.Accumulating.eitherConsAccumulatingSupport[String]


  val cos2 = support.product(support.product(Right(1), Right(2)), Right(3))

  // DebugMacros.code(PartialProductTransformations.transform[Option, Person, PersonRefined](person))

  println(

  DebugMacros.code(PartialProductTransformations.transformAccumulating[[A] =>> Either[::[String], A], Person, PersonRefined](person))
  )

  // println(cos)
}
