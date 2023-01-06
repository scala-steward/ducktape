package io.github.arainko.ducktape

import io.github.arainko.ducktape.ReproTransformer

case class A(anotherCaseClass: A.AnotherCaseClass)

object A {
  case class AnotherCaseClass(name: String)
  case class B(anotherCaseClass: AnotherCaseClass)
}

object ReproFile2 {
  ReproTransformer.derived[A, A.B]
  Transformer.forProducts[A, A.B]
}
