package io.github.arainko.ducktape.internal.macros

import io.github.arainko.ducktape.fallible.Accumulating
import io.github.arainko.ducktape.function.FunctionMirror
import io.github.arainko.ducktape.internal.modules.*
import io.github.arainko.ducktape.internal.util.NonEmptyList
import io.github.arainko.ducktape.{ BuilderConfig, FallibleBuilderConfig }

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*
import scala.util.chaining.*

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

    createTransformation[F, Source, Dest](F, sourceValue, Fields.dest.value, Nil, Nil)(Constructor.construct[Dest])
  }

  def transformConfigured[F[+x]: Type, Source: Type, Dest: Type](
    Source: Expr[Mirror.ProductOf[Source]],
    Dest: Expr[Mirror.ProductOf[Dest]],
    F: Expr[Accumulating.Support[F]],
    config: Expr[Seq[FallibleBuilderConfig[F, Source, Dest] | BuilderConfig[Source, Dest]]],
    sourceValue: Expr[Source]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    given Fields.Source = Fields.Source.fromMirror(Source)
    given Fields.Dest = Fields.Dest.fromMirror(Dest)

    val materializedConfig = MaterializedConfiguration.materializeFallibleProductConfig(config)
    val nonConfiguredFields = (Fields.dest.byName -- materializedConfig.map(_.destFieldName)).values.toList
    val (wrappedFields, unwrappedFields) = configuredFieldTransformations(materializedConfig, sourceValue)

    createTransformation(F, sourceValue, nonConfiguredFields, unwrappedFields, wrappedFields)(Constructor.construct[Dest])
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

        createTransformation[F, Source, Dest](F, sourceValue, Fields.dest.value, Nil, Nil) { unwrappedFields =>
          val rearrangedFields = rearrangeFieldsToDestOrder(unwrappedFields).map(_.value.asTerm)
          Select.unique(func, "apply").appliedToArgs(rearrangedFields).asExprOf[Dest]
        }
      case other => report.errorAndAbort(s"'via' is only supported on eta-expanded methods!")
    }
  }

  private def createTransformation[F[+x]: Type, Source: Type, Dest: Type](
    F: Expr[Accumulating.Support[F]],
    sourceValue: Expr[Source],
    fieldsToTransformInto: List[Field],
    unwrappedFieldsFromConfig: List[Field.Unwrapped],
    wrappedFieldsFromConfig: List[Field.Wrapped[F]]
  )(construct: List[Field.Unwrapped] => Expr[Dest])(using Quotes, Fields.Source) = {
    import quotes.reflect.*

    // Ideally .partition would work but if I deconstruct these two into tuples based on the subtype both of their parts are inferred as the union
    // anyway, hence this thing:
    val wrappedFields = List.newBuilder[Field.Wrapped[F]].addAll(wrappedFieldsFromConfig)
    val unwrappedFields = List.newBuilder[Field.Unwrapped].addAll(unwrappedFieldsFromConfig)

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
      .mapResult(NonEmptyList.fromList)
      .result()
      .map { transformedFields => ZippedProduct.zipAndConstruct(F, transformedFields, unwrappedFields.result())(construct) }
      .getOrElse('{ $F.pure(${ construct(unwrappedFields.result()) }) })
  }

  private def configuredFieldTransformations[F[+x]: Type, Source: Type](
    configs: List[MaterializedConfiguration.FallibleProduct[F]],
    sourceValue: Expr[Source]
  )(using Quotes, Fields.Dest) = {
    import quotes.reflect.*
    import MaterializedConfiguration.*

    val wrappedFields = List.newBuilder[Field.Wrapped[F]]
    val unwrappedFields = List.newBuilder[Field.Unwrapped]

    configs.foreach { cfg =>
      (Fields.dest.unsafeGet(cfg.destFieldName) -> cfg) match {
        case (field, FallibleProduct.Const(_, value)) =>
          wrappedFields += Field.Wrapped(field, value)
        case (field, FallibleProduct.Computed(_, function)) =>
          wrappedFields += Field.Wrapped(field, '{ $function($sourceValue) })
        case (field, FallibleProduct.Total(Product.Const(_, value))) =>
          unwrappedFields += Field.Unwrapped(field, value)
        case (field, FallibleProduct.Total(Product.Computed(_, function))) =>
          unwrappedFields += Field.Unwrapped(field, '{ $function($sourceValue) })
        case (field, FallibleProduct.Total(Product.Renamed(destField, sourceField))) =>
          unwrappedFields += Field.Unwrapped(field, sourceValue.accessFieldByName(sourceField).asExpr)
      }
    }

    wrappedFields.result() -> unwrappedFields.result()
  }

  private def rearrangeFieldsToDestOrder(fields: List[Field.Unwrapped])(using Fields.Dest) = {
    val unwrappedByName = fields.map(field => field.underlying.name -> field).toMap
    Fields.dest.value.map(field => unwrappedByName(field.name))
  }

}
