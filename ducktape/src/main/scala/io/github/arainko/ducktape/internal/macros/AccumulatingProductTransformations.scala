package io.github.arainko.ducktape.internal.macros

import scala.quoted.*
import io.github.arainko.ducktape.internal.modules.*
import scala.deriving.Mirror
import scala.annotation.tailrec
import scala.util.chaining.*

import io.github.arainko.ducktape.Accumulating

object AccumulatingProductTransformations {
  def transform[F[+x]: Type, Source: Type, Dest: Type](
    Source: Expr[Mirror.ProductOf[Source]],
    Dest: Expr[Mirror.ProductOf[Dest]],
    F: Expr[Accumulating.Support[F]],
    sourceValue: Expr[Source]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    given Fields.Source = Fields.Source.fromMirror(Source)
    given Fields.Dest = Fields.Dest.fromMirror(Dest)

    accumulatingFieldTransformations[F, Source, Dest](F, sourceValue, Fields.dest.value)
  }

  inline def transformAccumulating[F[+x], Source, Dest](
    sourceValue: Source
  )(using F: Accumulating.Support[F], Source: Mirror.ProductOf[Source], Dest: Mirror.ProductOf[Dest]) =
    ${ transform[F, Source, Dest]('Source, 'Dest, 'F, 'sourceValue) }

  private def accumulatingFieldTransformations[F[+x]: Type, Source: Type, Dest: Type](
    F: Expr[Accumulating.Support[F]],
    sourceValue: Expr[Source],
    fieldsToTransformInto: List[Field]
  )(using Quotes, Fields.Source) = {
    import quotes.reflect.*

    // Ideally .partition would work but if I deconstruct these two into tuples based on the subtype both of their parts are inferred as the union
    // anyway, hence this thing:
    val wrappedFields = List.newBuilder[Field.Wrapped[F]]
    val unwrappedFields = List.newBuilder[Field.Unwrapped]

    fieldsToTransformInto.foreach { dest =>
      val source =
        Fields.source
          .get(dest.name)
          .getOrElse(Failure.abort(Failure.NoFieldMapping(dest.name, summon[Type[Source]])))

      source.partialTransformerTo[F, Accumulating](dest).asExpr match {
        case '{ Accumulating.partialFromTotal[F, src, dest](using $total, $support) } =>
          val sourceField = sourceValue.accessField(source).asExprOf[src]
          val transformed = LiftTransformation.liftTransformation[src, dest](total, sourceField)
          unwrappedFields += Field.Unwrapped(dest, transformed)
        case '{ $transformer: Accumulating[F, src, dest] } =>
          val sourceField = sourceValue.accessField(source).asExprOf[src]
          wrappedFields += Field.Wrapped(dest, '{ $transformer.transform($sourceField) })
      }
    }

    wrappedFields
      .mapResult(wrapped => Option.when(wrapped.nonEmpty)(::(wrapped.head, wrapped.tail)))
      .result()
      .map { transformedFields =>
        zipFields[F, Dest](F, transformedFields) match {
          case '{ $zipped: F[a] } =>
            '{ $F.map($zipped, value => ${ unzipAndConstruct[Dest](transformedFields, unwrappedFields.result(), 'value) }) }
        }
      }
      .getOrElse('{ $F.pure(${ Constructor.construct[Dest](unwrappedFields.result()) }) })
  }

  private def zipFields[F[+x]: Type, Dest: Type](
    F: Expr[Accumulating.Support[F]],
    wrappedFields: NonEmptyList[Field.Wrapped[F]]
  )(using Quotes): Expr[F[Any]] =
    wrappedFields.map(_.value).reduceLeft { (accumulated, current) =>
      (accumulated -> current) match {
        case '{ $accumulated: F[a] } -> '{ $current: F[b] } =>
          '{ $F.product[`a`, `b`]($accumulated, $current) }
      }
    }

  private def unzipAndConstruct[Dest: Type](
    wrappedFields: NonEmptyList[Field.Wrapped[?]],
    unwrappedFields: List[Field.Unwrapped],
    nestedPairs: Expr[Any]
  )(using Quotes) = {
    import quotes.reflect.*

    val (pattern, unzippedFields) = ZippedProduct.unzip(nestedPairs, wrappedFields)

    // workaround for https://github.com/lampepfl/dotty/issues/16784
    val matchErrorBind = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, TypeRepr.of[Any])
    val matchErrorCase =
      CaseDef(Bind(matchErrorBind, Wildcard()), None, '{ throw new MatchError(${ Ref(matchErrorBind).asExpr }) }.asTerm)

    Match(
      nestedPairs.asTerm,
      CaseDef(pattern, None, Constructor.construct[Dest](unzippedFields ::: unwrappedFields).asTerm) :: matchErrorCase :: Nil
    ).asExprOf[Dest]
  }

  private type NonEmptyList[+A] = ::[A]
}
