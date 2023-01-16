package io.github.arainko.ducktape.internal.modules

import io.github.arainko.ducktape.Transformer

import scala.quoted.*
import io.github.arainko.ducktape.PartialTransformer

private[ducktape] final class Field(val name: String, val tpe: Type[?]) {
  def transformerTo(that: Field)(using Quotes): Expr[Transformer[?, ?]] = {
    import quotes.reflect.*

    (tpe -> that.tpe) match {
      case '[src] -> '[dest] =>
        Implicits.search(TypeRepr.of[Transformer[src, dest]]) match {
          case success: ImplicitSearchSuccess => success.tree.asExprOf[Transformer[src, dest]]
          case err: ImplicitSearchFailure     => report.errorAndAbort(err.explanation)
        }
    }
  }

  //This untyped due to not being able to reduce a HKT with wildcards
  def partialTransformerTo[
    F[+x]: Type,
    PartialTransformer[f[+x], a, b] <: PartialTransformer.FailFast[f, a, b] | PartialTransformer.Accumulating[f, a, b]: Type
  ](that: Field)(using Quotes): quotes.reflect.Term = {
    import quotes.reflect.*

    (tpe -> that.tpe) match {
      case '[src] -> '[dest] =>
        Implicits.search(TypeRepr.of[PartialTransformer[F, src, dest]]) match {
          case success: ImplicitSearchSuccess => success.tree
          case err: ImplicitSearchFailure => report.errorAndAbort(err.explanation)
        }
    }
  }

}
