package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.*
import io.github.arainko.ducktape.internal.macros.*
import scala.runtime.TupleMirror

import io.github.arainko.ducktape.internal.macros.FailFastProductTransformations

import io.github.arainko.ducktape.Accumulating

final case class Person(name: String, age: Int, additional: Int, int: Int, p: Person2)

final case class Person2(name: String, age: Int, additional: Int, int: Int)

final case class Person3(name: String, age: Int, additional: newtypes.Age, int: newtypes.Age)

final case class PersonRefined(name: newtypes.Name, age: Int, additional: Option[newtypes.Age], int: Int, p: Person3)


object newtypes {
  opaque type Name = String

  object Name {
    given refineName: FailFast[Option, String, Name] = Some(_)
    given refineName2: Accumulating[[A] =>> Either[::[String], A], String, Name] = 
      a => Left(::("WOOPS", Nil))
  }

  opaque type Age = Int

  object Age {
    given refineAge: FailFast[Option, Int, Age] = _ => None
    given refineAge2: Accumulating[[A] =>> Either[::[String], A], Int, Age] = a => Left(::("WOOPS2", Nil))
  }
}

object Playground extends App {
  val person = Person("name", 1, 1, 1, Person2("name", 1, 1, 1))


  // weird error about a non-found proxy for Option here...
  // DebugMacros.code(FailFastProductTransformations.transformFailFast[Option, Person, PersonRefined](person))
  
  /*
  given instance derived in trait DerivedAccumulatingTransformers does not match type io.github.arainko.ducktape.Accumulating[[A] =>> Either[::[String], A], 
    [error]    |  io.github.arainko.ducktape.Person2
    [error]    |, io.github.arainko.ducktape.Person3]

    When 'eitherStr' doesn't exist in Support.Accumulating... 
  */

  DebugMacros.code(Accumulating.derived[[A] =>> Either[::[String], A], Person, PersonRefined])
}
