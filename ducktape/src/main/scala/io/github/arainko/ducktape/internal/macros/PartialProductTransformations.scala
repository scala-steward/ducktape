package io.github.arainko.ducktape.internal.macros

import scala.quoted.*
import io.github.arainko.ducktape.PartialTransformer
import io.github.arainko.ducktape.internal.modules.*
import scala.deriving.Mirror
import scala.annotation.tailrec

object PartialProductTransformations {
  def transformFailFast[F[+x]: Type, Source: Type, Dest: Type](
    Source: Expr[Mirror.ProductOf[Source]],
    Dest: Expr[Mirror.ProductOf[Dest]],
    support: Expr[PartialTransformer.FailFast.Support[F]],
    sourceValue: Expr[Source]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    given Fields.Source = Fields.Source.fromMirror(Source)
    given Fields.Dest = Fields.Dest.fromMirror(Dest)

    failFastFieldTransformations[F, Source, Dest](support, sourceValue, Fields.dest.value)
  }

  inline def transform[F[+x], Source, Dest](
    sourceValue: Source
  )(using support: PartialTransformer.FailFast.Support[F], Source: Mirror.ProductOf[Source], Dest: Mirror.ProductOf[Dest]) =
    ${ transformFailFast[F, Source, Dest]('Source, 'Dest, 'support, 'sourceValue) }

  private def failFastFieldTransformations[F[+x]: Type, Source: Type, Dest: Type](
    support: Expr[PartialTransformer.FailFast.Support[F]],
    sourceValue: Expr[Source],
    fieldsToTransformInto: List[Field]
  )(using Quotes, Fields.Source) = {
    import quotes.reflect.*

    val transformedFields =
      fieldsToTransformInto.map { dest =>
        val source =
          Fields.source
            .get(dest.name)
            .getOrElse(Failure.abort(Failure.NoFieldMapping(dest.name, summon[Type[Source]])))

        source.partialTransformerTo[F, PartialTransformer.FailFast](dest).asExpr match {
          case '{ $transformer: PartialTransformer.FailFast[F, src, dest] } =>
            val sourceField = sourceValue.accessField(source).asExprOf[src]
            WrappedField(dest.name, '{ $transformer.transform($sourceField) })
        }
      }

    nestFlatMaps[F, Dest](support, transformedFields)
  }

  private def accumulatingFieldTransformations[F[+x]: Type, Source: Type, Dest: Type](
    support: Expr[PartialTransformer.Accumulating.Support[F]],
    sourceValue: Expr[Source],
    fieldsToTransformInto: List[Field]
  )(using Quotes, Fields.Source) = {
    import quotes.reflect.*

    val transformedFields =
      fieldsToTransformInto.map { dest =>
        val source =
          Fields.source
            .get(dest.name)
            .getOrElse(Failure.abort(Failure.NoFieldMapping(dest.name, summon[Type[Source]])))

        source.partialTransformerTo[F, PartialTransformer.FailFast](dest).asExpr match {
          case '{ $transformer: PartialTransformer.FailFast[F, src, dest] } =>
            val sourceField = sourceValue.accessField(source).asExprOf[src]
            WrappedField(dest.name, '{ $transformer.transform($sourceField) })
        }
      }

    // zipFields[F, Dest](support, transformedFields)
    ???
  }

  private def nestFlatMaps[F[+x]: Type, Dest: Type](
    support: Expr[PartialTransformer.FailFast.Support[F]],
    wrappedFields: List[WrappedField[F]]
  )(using Quotes): Expr[F[Dest]] = {
    def recurse(
      leftoverWrappedFields: List[WrappedField[F]],
      collectedUnwrappedFields: List[UnwrappedField]
    )(using Quotes): Expr[F[Dest]] =
      leftoverWrappedFields match {
        case WrappedField(name, value) :: Nil =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $support.map[`destField`, Dest]($value, a => ${ construct(UnwrappedField(name, 'a) :: collectedUnwrappedFields) })
              }
          }

        case WrappedField(name, value) :: next =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $support.flatMap[`destField`, Dest](
                  $value,
                  a => ${ recurse(next, UnwrappedField(name, 'a) :: collectedUnwrappedFields) }
                )
              }
          }

        case Nil =>
          val constructedValue = construct(collectedUnwrappedFields)
          '{ $support.pure[Dest]($constructedValue) }
      }

    recurse(wrappedFields, Nil)
  }

  private def zipFields[F[+x]: Type, Dest: Type](
    support: Expr[PartialTransformer.Accumulating.Support[F]],
    wrappedFields: List[WrappedField[F]]
  )(using Quotes): Option[Expr[F[Any]]] =
    wrappedFields.map(_.value).reduceLeftOption { (accumulated, current) =>
      (accumulated -> current) match {
        case '{ $accumulated: F[a] } -> '{ $current: F[b] } =>
          '{ $support.product($accumulated, $current) }
      }
    }

  private def construct[Dest: Type](fieldValues: List[UnwrappedField])(using Quotes) = {
    import quotes.reflect.*
    val namedArgs = fieldValues.map(field => NamedArg(field.name, field.value.asTerm))
    Constructor(TypeRepr.of[Dest]).appliedToArgs(namedArgs).asExprOf[Dest]
  }

  private final case class WrappedField[F[+x]](name: String, value: Expr[F[Any]])

  private final case class UnwrappedField(name: String, value: Expr[Any])

  private def unnestPairs(fields: List[Field], nestedPairs: Expr[Any])(using Quotes) = {
    import quotes.reflect.*

    val Tuple2Extractor = Select.unique('{ Tuple2 }.asTerm, "unapply")

    /*
    Tuple2(Tuple2(Tuple2(field1, field2), field3), field4)

    Unapply(Tuple2Extractor, Nil, Unapply(Tuple2Extractor, Nil, Unapply(Tuple2Extractor, Nil, field1 :: field2 :: Nil) :: field3 :: Nil) :: field4 :: Nil)

    so, you need to start generation from the last elem
    if there are two elems left return an Unapply with two binds instead of recursing
     */

    def recurse(
      fields: List[Field],
      collectedFields: List[UnwrappedField]
    )(using Quotes): Option[Unapply | Bind] =
      fields match {
        case first :: second :: Nil => // Unapply with two binds
          val firstBind = Symbol.newBind(Symbol.spliceOwner, first.name, Flags.Local, TypeRepr.of(using first.tpe))
          val secondBind = Symbol.newBind(Symbol.spliceOwner, second.name, Flags.Local, TypeRepr.of(using second.tpe))
          Some(Unapply(Tuple2Extractor, Nil, Bind(secondBind, Wildcard()) :: Bind(firstBind, Wildcard()) :: Nil))

        case single :: Nil =>
          val bind = Symbol.newBind(Symbol.spliceOwner, single.name, Flags.Local, TypeRepr.of(using single.tpe))
          Some(Bind(bind, Wildcard()))

        case single :: next =>
          val bind = Symbol.newBind(Symbol.spliceOwner, single.name, Flags.Local, TypeRepr.of(using single.tpe))
          recurse(next, collectedFields).map(pattern => Unapply(Tuple2Extractor, Nil, pattern :: Bind(bind, Wildcard()) :: Nil))

        case Nil => None
      }

    recurse(fields.reverse, Nil).map(pattern => Match(nestedPairs.asTerm, List(CaseDef(pattern, None, Literal(IntConstant(1))))))
  }

  def unnestTuple(value: Expr[Any])(using Quotes) = {
    import quotes.reflect.*

    val fields = List(
      Field("elem1", Type.of[Int]),
      Field("elem2", Type.of[Int]),
      Field("elem3", Type.of[Int]),
      Field("elem4", Type.of[Int]),
      Field("elem5", Type.of[Int]),
    )

    unnestPairs(fields, value).get.asExprOf[Int]
  }

  inline def unnestUsage(value: Any) = ${ unnestTuple('value) }
}
