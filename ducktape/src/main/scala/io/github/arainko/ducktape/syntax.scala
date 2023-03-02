package io.github.arainko.ducktape

import io.github.arainko.ducktape.builder.*
import io.github.arainko.ducktape.function.*
import io.github.arainko.ducktape.internal.macros.*
import io.github.arainko.ducktape.internal.modules.*

import scala.deriving.Mirror
import io.github.arainko.ducktape.internal.macros.Transformations.AccumulatingViaPartiallyApplied

extension [Source](value: Source) {
  def into[Dest]: AppliedBuilder[Source, Dest] = AppliedBuilder(value)

  def failFastInto[Dest] = ???

  def accumulatingInto[Dest] = ???

  def to[Dest](using Transformer[Source, Dest]): Dest = Transformer[Source, Dest].transform(value)

  def failFastTo[F[+x], Dest](using failFast: Transformer.FailFast[F, Source, Dest]): F[Dest] = failFast.transform(value)

  def accumulatingTo[F[+x], Dest](using accumulating: Transformer.Accumulating[F, Source, Dest]): F[Dest] =
    accumulating.transform(value)

  transparent inline def intoVia[Func](inline function: Func)(using Mirror.ProductOf[Source], FunctionMirror[Func]) =
    AppliedViaBuilder.create(value, function)

  def failFastIntoVia[Func] = ???

  def accumulatingIntoVia[Func] = ???

  inline def via[Func](inline function: Func)(using
    Func: FunctionMirror[Func],
    Source: Mirror.ProductOf[Source]
  ): Func.Return = Transformations.via(value, function)

  def failFastVia[Func] = ???

  def accumulatingVia[F[+x]]: Transformations.PartiallyApplied[F, Source] =
    Transformations.PartiallyApplied[F, Source](value)
}
