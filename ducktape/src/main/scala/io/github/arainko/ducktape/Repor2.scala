package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.macros.*
import io.github.arainko.ducktape.fallible.FallibleTransformer
import io.github.arainko.ducktape.function.FunctionMirror
import scala.deriving.Mirror

extension [F[+x], Source](value: Source)(using F: Transformer.Mode[F]) {

  inline def fallibleVia2[Func](inline function: Func)(using
    Func: FunctionMirror[Func]
  )(using Source: Mirror.ProductOf[Source]) = 
    inline F match {
      case given fallible.Mode.FailFast[F] =>
        Errors.repro
      case given fallible.Mode.Accumulating[F] =>
        Errors.repro
      case other =>
        Errors.repro
    }

}