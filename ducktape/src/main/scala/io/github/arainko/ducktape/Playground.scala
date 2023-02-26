package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.*
import io.github.arainko.ducktape.internal.macros.*
import io.github.arainko.ducktape.partial.{ Accumulating, FailFast }
import io.github.arainko.ducktape.newtypes.Age
import io.github.arainko.ducktape.newtypes.Name
import scala.deriving.Mirror

final case class Person(name: String, age: Int, additional: Int, int: Int, p: Person2)

final case class Person2(name: String, age: Int, additional: Int, int: Int)

final case class Person3(name: String, age: Int, additional: newtypes.Age, int: newtypes.Age)

final case class PersonRefined(name: newtypes.Name, age: Int, additional: Option[newtypes.Age], int: Int, p: Person3)

object newtypes {
  opaque type Name = String

  object Name {
    given refineNameFf: FailFast[Option, String, Name] = Some(_)
    given refineNameAcc: Accumulating[[A] =>> Either[::[String], A], String, Name] =
      a => Left(::("WOOPS", Nil))
  }

  opaque type Age = Int

  object Age {
    given refineAgeFf: FailFast[Option, Int, Age] = _ => None
    given refineAgeAcc: Accumulating[[A] =>> Either[::[String], A], Int, Age] = a => Left(::("WOOPS2", Nil))
  }
}

object Playground extends App {
  val person = Person("name", 1, 1, 1, Person2("name", 1, 1, 1))


  // PersonRefined.apply()

  // private given cos: FunctionMirror[(Name, Int, Option[Age], Int, Person3) => PersonRefined]{type Return >: PersonRefined <: PersonRefined} = summon[FunctionMirror[(Name, Int, Option[Age], Int, Person3) => PersonRefined]]

  // Transformations.via(person, PersonRefined.apply)

  // given FunctionMirror.Aux[(Name, Int, Option[Age], Int, Person3) => PersonRefined, PersonRefined] = ???

  val cos: Either[::[String], PersonRefined] = 
    DebugMacros.code(Transformations.AccumulatingViaPartiallyApplied[[A] =>> Either[::[String], A]].apply(PersonRefined.apply)(person))

  // val cos: Either[::[String], PersonRefined] =
  // Transformations.accumulatingVia[[A] =>> Either[::[String], A], Person, PersonRefined, (Name, Int, Option[Age], Int, Person3) => PersonRefined](person, PersonRefined.apply)

  // DebugMacros.code(summon[Accumulating[[A] =>> Either[::[String], A], Person, PersonRefined]])
  // DebugMacros.code(summon[FailFast[[A] =>> Option[A], Person, PersonRefined]])
  // DebugMacros.code(Accumulating.derived[[A] =>> Either[::[String], A], Person, PersonRefined])
}
