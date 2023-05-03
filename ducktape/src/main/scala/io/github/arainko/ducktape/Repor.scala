package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.macros.*
import io.github.arainko.ducktape.fallible.FallibleTransformer
import io.github.arainko.ducktape.function.FunctionMirror
import scala.deriving.Mirror
import io.github.arainko.ducktape.fallible.Mode.Accumulating
import io.github.arainko.ducktape.fallible.Mode.FailFast

extension (value: Int) inline def costam3 = Transformations.repro
