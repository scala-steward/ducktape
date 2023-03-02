package io.github.arainko.ducktape.internal.macros

import io.github.arainko.ducktape.internal.modules.*
import io.github.arainko.ducktape.partial.Accumulating

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*
import scala.util.chaining.*
import io.github.arainko.ducktape.function.FunctionMirror

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

    createTransformation[F, Source, Dest](F, sourceValue, Fields.dest.value)(Constructor.construct[Dest])
  }

  def via[F[+x]: Type, Source: Type, Dest: Type, Func](
    sourceValue: Expr[Source],
    function: Expr[Func],
    Source: Expr[Mirror.ProductOf[Source]],
    F: Expr[Accumulating.Support[F]]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    function.asTerm match {
      case func @ FunctionLambda(vals, _) =>
        given Fields.Source = Fields.Source.fromMirror(Source)
        given Fields.Dest = Fields.Dest.fromValDefs(vals)

        createTransformation[F, Source, Dest](F, sourceValue, Fields.dest.value) { unwrappedFields =>
          val rearrangedFields = rearrangeFieldsToDestOrder(unwrappedFields).map(_.value.asTerm)
          Select.unique(func, "apply").appliedToArgs(rearrangedFields).asExprOf[Dest]
        }
      case other => report.errorAndAbort(s"'via' is only supported on eta-expanded methods!")
    }
  }

  private def createTransformation[F[+x]: Type, Source: Type, Dest: Type](
    F: Expr[Accumulating.Support[F]],
    sourceValue: Expr[Source],
    fieldsToTransformInto: List[Field]
  )(construct: List[Field.Unwrapped] => Expr[Dest])(using Quotes, Fields.Source) = {
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
            '{
              $F.map(
                $zipped,
                value => ${ unzipAndConstruct[Dest](transformedFields, unwrappedFields.result(), 'value, construct) }
              )
            }
        }
      }
      .getOrElse('{ $F.pure(${ construct(unwrappedFields.result()) }) })
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
    nestedPairs: Expr[Any],
    construct: List[Field.Unwrapped] => Expr[Dest]
  )(using Quotes) = {
    import quotes.reflect.*

    ZippedProduct.unzip(nestedPairs, wrappedFields) match {
      case (bind: Bind, unzippedFields) =>
        Match(
          nestedPairs.asTerm,
          CaseDef(
            bind,
            None,
            construct(unzippedFields ::: unwrappedFields).asTerm
          ) :: Nil
        ).asExprOf[Dest]

      case (pattern: Unapply, unzippedFields) =>
        // workaround for https://github.com/lampepfl/dotty/issues/16784
        val matchErrorBind = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, TypeRepr.of[Any])
        val wronglyMatchedReference = Ref(matchErrorBind).asExpr
        val matchErrorCase =
          CaseDef(Bind(matchErrorBind, Wildcard()), None, '{ throw new MatchError($wronglyMatchedReference) }.asTerm)

        Match(
          nestedPairs.asTerm,
          CaseDef(
            pattern,
            None,
            construct(unzippedFields ::: unwrappedFields).asTerm
          ) :: matchErrorCase :: Nil
        ).asExprOf[Dest]
    }

  }

  private def rearrangeFieldsToDestOrder(fields: List[Field.Unwrapped])(using Fields.Dest) = {
    val unwrappedByName = fields.map(field => field.underlying.name -> field).toMap
    Fields.dest.value.map(field => unwrappedByName(field.name))
  }

  private type NonEmptyList[+A] = ::[A]
}
