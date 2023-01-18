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

  private def unnestPairs[F[+x]: Type](fields: List[Field], nestedPairs: Expr[F[Any]])(using Quotes) = {
    import quotes.reflect.*

    val destructor = Select.unique('{ Tuple2 }.asTerm, "unapply")

    def recurse(fields: List[Field], collectedFields: List[UnwrappedField], unapplyPatterns: List[Term])(using Quotes): (List[UnwrappedField], Option[Unapply]) =
      fields match {
        case field :: Nil => 
          val bindSymbol = Symbol.newBind(Symbol.spliceOwner, field.name, Flags.Local, TypeRepr.of(using field.tpe))
          val unwrappedFields = UnwrappedField(field.name, Ref(bindSymbol).asExpr) :: collectedFields
          unwrappedFields -> Some(Bind(bindSymbol, Wildcard()) :: unapplyPatterns))
        case field :: next => 
          val bindSymbol = Symbol.newBind(Symbol.spliceOwner, field.name, Flags.Local, TypeRepr.of(using field.tpe))
          val unwrappedValue = Ref(bindSymbol).asExpr
          val currentUnapply = recurse(next, UnwrappedField(field.name, unwrappedValue) :: collectedFields)

        case Nil => collectedFields -> None
      }

  }

  def unnestTuple(value: Expr[((Int, Int), Int)])(using Quotes) = {
    import quotes.reflect.*

    val destructor = Select.unique('{ Tuple2 }.asTerm, "unapply")

    val term = value.asTerm

    val elem1 = Symbol.newBind(Symbol.spliceOwner, "elem1", Flags.Local, TypeRepr.of[Int])
    val elem2 = Symbol.newBind(Symbol.spliceOwner, "elem2", Flags.Local, TypeRepr.of[Int])
    val elem3 = Symbol.newBind(Symbol.spliceOwner, "elem3", Flags.Local, TypeRepr.of[Int])

    val destruct = 
      Unapply(
        destructor,
        Nil,
        List(
          Unapply(destructor, Nil, List(Bind(elem1, Wildcard()), Bind(elem2, Wildcard()))),
          Bind(elem3, Wildcard())
        )
      )

    Match(
      term,
      List(
        CaseDef(
          destruct,
          None,
          Ref(elem1)
        )
      )
    ).asExprOf[Int]
  }

  inline def unnestUsage(value: ((Int, Int), Int)) = ${ unnestTuple('value) }
}
