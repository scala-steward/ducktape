package io.github.arainko.ducktape.internal.macros

import io.github.arainko.ducktape.*
import io.github.arainko.ducktape.function.*
import io.github.arainko.ducktape.internal.macros.{ CoproductTransformations, LiftTransformation }
import io.github.arainko.ducktape.internal.modules.*

import scala.deriving.Mirror
import scala.quoted.*

private[ducktape] object Transformations {
  inline def via[Source, Dest, Func](source: Source, inline function: Func)(using
    Source: Mirror.ProductOf[Source],
    Func: FunctionMirror.Aux[Func, Dest]
  ): Dest = ${ ProductTransformations.via('source, 'function, 'Func, 'Source) }

  // inline def accumulatingVia[F[+x], Source, Dest, Func](inline function: Func)(using
  //   Func: FunctionMirror.Aux[Func, Dest]
  // )(source: Source)(using Source: Mirror.ProductOf[Source], F: Transformer.Accumulating.Support[F]): F[Dest] = ${
  //   AccumulatingProductTransformations.via('source, 'function, 'Func, 'Source, 'F)
  // }

  // alternative to AccumulatingViaPartiallyApplied
  opaque type PartiallyApplied[F[+x], Source] = Source

  object PartiallyApplied {
    def apply[F[+x], Source](src: Source): PartiallyApplied[F, Source] = src

    extension [F[+x], Source](self: PartiallyApplied[F, Source]) {
      inline def apply[Func](inline function: Func)(using
        Func: FunctionMirror[Func]
      )(using Source: Mirror.ProductOf[Source], F: Transformer.Accumulating.Support[F]): F[Func.Return] = ${
        AccumulatingProductTransformations.via[F, Source, Func.Return, Func]('self, 'function, 'Source, 'F)
      }
    }
  }

  final class AccumulatingViaPartiallyApplied[F[+x], Source](source: Source) {
    inline def apply[Func](inline function: Func)(using
      Func: FunctionMirror[Func]
    )(using Source: Mirror.ProductOf[Source], F: Transformer.Accumulating.Support[F]): F[Func.Return] = ${
      AccumulatingProductTransformations.via[F, Source, Func.Return, Func]('source, 'function, 'Source, 'F)
    }
  }

  final class FailFastViaPartiallyApplied[F[+x], Source](source: Source) {
    inline def apply[Func](inline function: Func)(using
      Func: FunctionMirror[Func]
    )(using Source: Mirror.ProductOf[Source], F: Transformer.FailFast.Support[F]): F[Func.Return] = ${
      FailFastProductTransformations.via[F, Source, Func.Return, Func]('source, 'function, 'Source, 'F)
    }
  }

  inline def partially[Source, Dest, Func] = [f[+x]] => (t: Int) => t

  inline def viaConfigured[Source, Dest, Func, ArgSelector <: FunctionArguments](
    source: Source,
    inline function: Func,
    inline config: ArgBuilderConfig[Source, Dest, ArgSelector]*
  )(using Source: Mirror.ProductOf[Source]): Dest =
    ${ ProductTransformations.viaConfigured[Source, Dest, Func, ArgSelector]('source, 'function, 'config, 'Source) }

  inline def liftFromTransformer[Source, Dest](source: Source)(using inline transformer: Transformer[Source, Dest]) =
    ${ LiftTransformation.liftTransformation[Source, Dest]('transformer, 'source) }

  inline def transformConfigured[Source, Dest](source: Source, inline config: BuilderConfig[Source, Dest]*) =
    ${ transformConfiguredMacro[Source, Dest]('source, 'config) }

  private def transformConfiguredMacro[Source: Type, Dest: Type](
    sourceValue: Expr[Source],
    config: Expr[Seq[BuilderConfig[Source, Dest]]]
  )(using Quotes): Expr[Dest] =
    mirrorOf[Source]
      .zip(mirrorOf[Dest])
      .collect {
        case '{ $source: Mirror.ProductOf[Source] } -> '{ $dest: Mirror.ProductOf[Dest] } =>
          ProductTransformations.transformConfigured(sourceValue, config, source, dest)
        case '{ $source: Mirror.SumOf[Source] } -> '{ $dest: Mirror.SumOf[Dest] } =>
          CoproductTransformations.transformConfigured(sourceValue, config, source, dest)
      }
      .getOrElse(
        quotes.reflect.report
          .errorAndAbort("Configured transformations are supported for Product -> Product and Coproduct -> Coproduct.")
      )

  private def mirrorOf[A: Type](using Quotes) = Expr.summon[Mirror.Of[A]]
}
