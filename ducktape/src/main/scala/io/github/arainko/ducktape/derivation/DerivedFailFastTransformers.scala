package io.github.arainko.ducktape.derivation

import scala.deriving.Mirror
import io.github.arainko.ducktape.internal.macros.*
import io.github.arainko.ducktape.FailFast

trait DerivedFailFastTransformers {
  inline given derived[F[+x], Source, Dest](using
    Source: Mirror.ProductOf[Source],
    Dest: Mirror.ProductOf[Dest],
    F: FailFast.Support[F]
  ): FailFast[F, Source, Dest] = DerivedTransformers.failFastProduct[F, Source, Dest]
}
