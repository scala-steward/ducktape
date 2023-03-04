package io.github.arainko.ducktape.builder

import io.github.arainko.ducktape.{ BuilderConfig, FallibleBuilderConfig }

final class AppliedFallibleFailFastBuilder[F[+x], Source, Dest](source: Source) {
  inline def transform(inline config: FallibleBuilderConfig[F, Source, Dest] | BuilderConfig[Source, Dest]*): F[Dest] = ???
}
