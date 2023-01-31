package io.github.arainko.ducktape

import scala.compiletime.*

object usage extends App {
  val works1 = summon[ReproTransformer[String, String]]
  val works2 = summonInline[ReproTransformer[String, String]]

  /*
    Exception occurred while executing macro expansion.
    dotty.tools.dotc.reporting.UnhandledError: too many arguments for constructor AnyVal in class AnyVal: (): AnyVal
    ...
  */
  val error = ReproTransformer.summon[String, String]
}

