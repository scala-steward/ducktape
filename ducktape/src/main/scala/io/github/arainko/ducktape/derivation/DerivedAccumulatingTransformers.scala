package io.github.arainko.ducktape.derivation

import scala.deriving.Mirror
import io.github.arainko.ducktape.internal.macros.*

import io.github.arainko.ducktape.Accumulating
trait DerivedAccumulatingTransformers {
  inline given derived[F[+x], Source, Dest](using
    Source: Mirror.ProductOf[Source],
    Dest: Mirror.ProductOf[Dest],
    F: Accumulating.Support[F]
  ): Accumulating[F, Source, Dest] = DerivedTransformers.accumulatingProduct[F, Source, Dest]
}
