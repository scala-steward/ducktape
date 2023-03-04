package io.github.arainko.ducktape.builder

import io.github.arainko.ducktape.{ BuilderConfig, FallibleBuilderConfig }

final class AppliedFallibleAccumulatingBuilder[F[+x], Source, Dest](source: Source) {
  inline def transform(inline config: FallibleBuilderConfig[F, Source, Dest] | BuilderConfig[Source, Dest]*): F[Dest] = ???
}
