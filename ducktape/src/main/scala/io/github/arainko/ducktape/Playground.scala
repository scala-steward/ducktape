package io.github.arainko.ducktape

import io.github.arainko.ducktape.internal.*
import io.github.arainko.ducktape.internal.macros.*

object Playground extends App {
  val cos = 
    Transformer.Debug.showCode(
    PartialProductTransformations.usage(Some(1), None, Some(2), Some(3))
    )

  val cos2 = 
    DebugMacros.structure(
    Some(1).zip(Some(2)).zip(Some(3)).map { case ((one, two), three) => List(one, two, three) }
    )

  val cos3 = DebugMacros.code(PartialProductTransformations.unnestUsage(1 -> 2 -> 3))

  println(cos)
}
