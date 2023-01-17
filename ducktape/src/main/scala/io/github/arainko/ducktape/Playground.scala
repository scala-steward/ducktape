package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.*
import io.github.arainko.ducktape.internal.macros.*

final case class Person(name: String, age: Int, additional: Int)

final case class PersonRefined(name: newtypes.Name)


object newtypes {
  opaque type Name = String

  object Name {
    given refineName: PartialTransformer.FailFast[Option, String, Name] = Some(_)
  }

  opaque type Age = Int

  object Age {
    given refineAge: PartialTransformer.FailFast[Option, Int, Age] = _ => None
  }
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

  // val cos3 = DebugMacros.code(PartialProductTransformations.unnestUsage(1 -> 2 -> 3))

  // val personOpt = PersonOpt(Some("name"), Some(1))
  val person = Person("name", 1, 1)

  DebugMacros.code(PartialProductTransformations.transform[Option, Person, PersonRefined](person))

  
  // println(cos)
}
