package io.github.arainko.ducktape.builder

import io.github.arainko.ducktape.{ BuilderConfig, FallibleBuilderConfig }
import io.github.arainko.ducktape.internal.macros.Transformations
import io.github.arainko.ducktape.Transformer
import scala.deriving.Mirror

final class AppliedFallibleAccumulatingBuilder[F[+x], Source, Dest](source: Source) {
  inline def transform(
    inline config: FallibleBuilderConfig[F, Source, Dest] | BuilderConfig[Source, Dest]*
  )(using F: Transformer.Accumulating.Support[F], Source: Mirror.ProductOf[Source], Dest: Mirror.ProductOf[Dest]): F[Dest] =
    Transformations.transformAccumulatingConfigured[F, Source, Dest](source, config*)
}
