package io.github.arainko.ducktape.internal.macros

import scala.quoted.*
import io.github.arainko.ducktape.PartialTransformer
import io.github.arainko.ducktape.internal.modules.*
import scala.deriving.Mirror
import scala.annotation.tailrec
import scala.util.chaining.*

object PartialProductTransformations {
  def transformFailFast[F[+x]: Type, Source: Type, Dest: Type](
    Source: Expr[Mirror.ProductOf[Source]],
    Dest: Expr[Mirror.ProductOf[Dest]],
    F: Expr[PartialTransformer.FailFast.Support[F]],
    sourceValue: Expr[Source]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    given Fields.Source = Fields.Source.fromMirror(Source)
    given Fields.Dest = Fields.Dest.fromMirror(Dest)

    failFastFieldTransformations[F, Source, Dest](F, sourceValue, Fields.dest.value)
  }

  def transformAcc[F[+x]: Type, Source: Type, Dest: Type](
    Source: Expr[Mirror.ProductOf[Source]],
    Dest: Expr[Mirror.ProductOf[Dest]],
    F: Expr[PartialTransformer.Accumulating.Support[F]],
    sourceValue: Expr[Source]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    given Fields.Source = Fields.Source.fromMirror(Source)
    given Fields.Dest = Fields.Dest.fromMirror(Dest)

    accumulatingFieldTransformations[F, Source, Dest](F, sourceValue, Fields.dest.value)
  }

  inline def transform[F[+x], Source, Dest](
    sourceValue: Source
  )(using F: PartialTransformer.FailFast.Support[F], Source: Mirror.ProductOf[Source], Dest: Mirror.ProductOf[Dest]) =
    ${ transformFailFast[F, Source, Dest]('Source, 'Dest, 'F, 'sourceValue) }

  inline def transformAccumulating[F[+x], Source, Dest](
    sourceValue: Source
  )(using F: PartialTransformer.Accumulating.Support[F], Source: Mirror.ProductOf[Source], Dest: Mirror.ProductOf[Dest]) =
    ${ transformAcc[F, Source, Dest]('Source, 'Dest, 'F, 'sourceValue) }

  private def failFastFieldTransformations[F[+x]: Type, Source: Type, Dest: Type](
    F: Expr[PartialTransformer.FailFast.Support[F]],
    sourceValue: Expr[Source],
    fieldsToTransformInto: List[Field]
  )(using Quotes, Fields.Source) = {
    import quotes.reflect.*

    val transformedFields =
      fieldsToTransformInto.map[Field.Wrapped[F] | Field.Unwrapped] { dest =>
        val source =
          Fields.source
            .get(dest.name)
            .getOrElse(Failure.abort(Failure.NoFieldMapping(dest.name, summon[Type[Source]])))

        source.partialTransformerTo[F, PartialTransformer.FailFast](dest).asExpr match {
          case '{ PartialTransformer.FailFast.partialFromTotal[F, src, dest](using $total, $support) } =>
            val sourceField = sourceValue.accessField(source).asExprOf[src]
            val lifted = LiftTransformation.liftTransformation[src, dest](total, sourceField)
            Field.Unwrapped(dest, lifted)
          case '{ $transformer: PartialTransformer.FailFast[F, src, dest] } =>
            val sourceField = sourceValue.accessField(source).asExprOf[src]
            Field.Wrapped(dest, '{ $transformer.transform($sourceField) })
        }
      }

    nestFlatMaps[F, Dest](F, transformedFields)
  }

  private def accumulatingFieldTransformations[F[+x]: Type, Source: Type, Dest: Type](
    F: Expr[PartialTransformer.Accumulating.Support[F]],
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

      source.partialTransformerTo[F, PartialTransformer.Accumulating](dest).asExpr match {
        case '{ PartialTransformer.Accumulating.partialFromTotal[F, src, dest](using $total, $support) } =>
          val sourceField = sourceValue.accessField(source).asExprOf[src]
          val transformed = LiftTransformation.liftTransformation[src, dest](total, sourceField)
          unwrappedFields += Field.Unwrapped(dest, transformed)
        case '{ $transformer: PartialTransformer.Accumulating[F, src, dest] } =>
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
      .getOrElse('{ $F.pure(${ construct[Dest](unwrappedFields.result()) }) })
  }

  private def nestFlatMaps[F[+x]: Type, Dest: Type](
    F: Expr[PartialTransformer.FailFast.Support[F]],
    fields: List[Field.Wrapped[F] | Field.Unwrapped]
  )(using Quotes): Expr[F[Dest]] = {
    def recurse(
      leftoverFields: List[Field.Wrapped[F] | Field.Unwrapped],
      collectedUnwrappedFields: List[Field.Unwrapped]
    )(using Quotes): Expr[F[Dest]] =
      leftoverFields match {
        case Field.Wrapped(field, value) :: Nil =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $F.map[`destField`, Dest]($value, a => ${ construct(Field.Unwrapped(field, 'a) :: collectedUnwrappedFields) })
              }
          }

        case Field.Wrapped(field, value) :: next =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $F.flatMap[`destField`, Dest](
                  $value,
                  a => ${ recurse(next, Field.Unwrapped(field, 'a) :: collectedUnwrappedFields) }
                )
              }
          }

        case (f: Field.Unwrapped) :: next =>
          recurse(next, f :: collectedUnwrappedFields)

        case Nil =>
          val constructedValue = construct(collectedUnwrappedFields)
          '{ $F.pure[Dest]($constructedValue) }
      }

    recurse(fields, Nil)
  }

  private def zipFields[F[+x]: Type, Dest: Type](
    F: Expr[PartialTransformer.Accumulating.Support[F]],
    wrappedFields: NonEmptyList[Field.Wrapped[F]]
  )(using Quotes): Expr[F[Any]] =
    wrappedFields.map(_.value).reduceLeft { (accumulated, current) =>
      (accumulated -> current) match {
        case '{ $accumulated: F[a] } -> '{ $current: F[b] } =>
          '{ $F.product[`a`, `b`]($accumulated, $current) }
      }
    }

  private def construct[Dest: Type](fieldValues: List[Field.Unwrapped])(using Quotes) = {
    import quotes.reflect.*
    val namedArgs = fieldValues.map(field => NamedArg(field.underlying.name, field.value.asTerm))
    Constructor(TypeRepr.of[Dest]).appliedToArgs(namedArgs).asExprOf[Dest]
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
      CaseDef(pattern, None, construct[Dest](unzippedFields ::: unwrappedFields).asTerm) :: matchErrorCase :: Nil
    ).asExprOf[Dest]
  }

  private type NonEmptyList[+A] = ::[A]
}
