package io.github.arainko.ducktape.internal.macros

import io.github.arainko.ducktape.*
import io.github.arainko.ducktape.fallible.Mode
import io.github.arainko.ducktape.function.*

import scala.deriving.Mirror
import scala.quoted.*

private[ducktape] object Transformations {
  inline def repro: String = "aASD"

  inline def transformConfigured =
    ???

  // def transformConfiguredMacro[Source: Type, Dest: Type](
  //   sourceValue: Expr[Source],
  //   config: Expr[Seq[BuilderConfig[Source, Dest]]]
  // )(using Quotes): Expr[Dest] =
  //   mirrorOf[Source]
  //     .zip(mirrorOf[Dest])
  //     .collect {
  //       case '{ $source: Mirror.ProductOf[Source] } -> '{ $dest: Mirror.ProductOf[Dest] } =>
  //         ProductTransformations.transformConfigured(sourceValue, config, source, dest)
  //       case '{ $source: Mirror.SumOf[Source] } -> '{ $dest: Mirror.SumOf[Dest] } =>
  //         CoproductTransformations.transformConfigured(sourceValue, config, source, dest)
  //     }
  //     .getOrElse(
  //       quotes.reflect.report
  //         .errorAndAbort("Configured transformations are supported for Product -> Product and Coproduct -> Coproduct.")
  //     )

  // def mirrorOf[A: Type](using Quotes) = Expr.summon[Mirror.Of[A]]
}
