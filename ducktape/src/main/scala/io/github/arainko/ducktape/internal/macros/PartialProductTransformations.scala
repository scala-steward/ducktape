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

    fieldTransformations[F, Source, Dest](support, sourceValue, Fields.dest.value)
  }

  inline def transform[F[+x], Source, Dest](
    sourceValue: Source
  )(using support: PartialTransformer.FailFast.Support[F], Source: Mirror.ProductOf[Source], Dest: Mirror.ProductOf[Dest]) =
    ${ transformFailFast[F, Source, Dest]('Source, 'Dest, 'support, 'sourceValue) }

  private def fieldTransformations[F[+x]: Type, Source: Type, Dest: Type](
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
            val sourceField = accessField(sourceValue, source.name).asExprOf[src]
            WrappedField(dest.name, '{ $transformer.transform($sourceField) })
        }
      }

    def nestFlatMaps(
      wrappedFields: List[WrappedField[F]],
      collectedValues: List[UnwrappedField]
    )(using Quotes): Expr[F[Dest]] = {
      import quotes.reflect.*

      wrappedFields match {
        case WrappedField(name, value) :: Nil =>
          value match {
            case '{ $value: F[destField] } =>
              '{ $support.map[`destField`, Dest]($value, a => ${ construct(UnwrappedField(name, 'a) :: collectedValues) }) }
          }

        case WrappedField(name, value) :: next =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $support.flatMap[`destField`, Dest](
                  $value,
                  a => ${ nestFlatMaps(next, UnwrappedField(name, 'a) :: collectedValues) }
                )
              }
          }

        case Nil =>
          val constructedValue = construct(collectedValues)
          '{ $support.pure[Dest]($constructedValue) }
      }
    }

    def construct(fieldValues: List[UnwrappedField])(using Quotes) = {
      import quotes.reflect.*
      val namedArgs = fieldValues.map(field => NamedArg(field.name, field.value.asTerm))
      constructor(TypeRepr.of[Dest]).appliedToArgs(namedArgs).asExprOf[Dest]
    }

    val term = nestFlatMaps(transformedFields, Nil).asTerm
    println()
    println(term.show(using Printer.TreeShortCode))
    println()
    term.asExprOf[F[Dest]]
  }

  private final case class WrappedField[F[+x]](name: String, value: Expr[F[Any]])

  private final case class UnwrappedField(name: String, value: Expr[Any])

  // TODO: Extract into a separate file
  private def accessField(value: Expr[Any], fieldName: String)(using Quotes) = {
    import quotes.reflect.*

    Select.unique(value.asTerm, fieldName)
  }

  // TODO: Copied code, extract to separate file
  private def constructor(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.Term = {
    import quotes.reflect.*

    val (repr, constructor, tpeArgs) =
      tpe match {
        case AppliedType(repr, reprArguments) => (repr, repr.typeSymbol.primaryConstructor, reprArguments)
        case notApplied                       => (tpe, tpe.typeSymbol.primaryConstructor, Nil)
      }

    New(Inferred(repr))
      .select(constructor)
      .appliedToTypes(tpeArgs)
  }

  def nestedFlatMaps(exprs: List[Expr[Option[Int]]], collectedValues: List[Expr[Int]])(using Quotes): Expr[Option[List[Int]]] = {
    import quotes.reflect.*

    // Tuple2.unapply()

    exprs match {
      case head :: next =>
        '{ $head.flatMap(a => ${ nestedFlatMaps(next, 'a :: collectedValues) }) }
      case Nil => '{ Some(${ Varargs(collectedValues.reverse) }.toList) }
    }

    // def loop[A: Type](acc: )(using Quotes)
  }

  def usageProxy(exprs: Expr[Seq[Option[Int]]])(using Quotes): Expr[Option[List[Int]]] = {
    val list = Varargs.unapply(exprs).get.toList
    nestedFlatMaps(list, Nil)
  }

  inline def usage(inline values: Option[Int]*): Option[List[Int]] =
    ${ usageProxy('values) }

  def unnestTuple(value: Expr[((Int, Int), Int)])(using Quotes) = {
    import quotes.reflect.*

    val destructor = Select.unique('{ Tuple2 }.asTerm, "unapply")

    val term = value.asTerm

    val elem1 = Symbol.newBind(Symbol.spliceOwner, "elem1", Flags.Local, TypeRepr.of[Int])
    val elem2 = Symbol.newBind(Symbol.spliceOwner, "elem2", Flags.Local, TypeRepr.of[Int])
    val elem3 = Symbol.newBind(Symbol.spliceOwner, "elem3", Flags.Local, TypeRepr.of[Int])

    val innerDestruct = Unapply(destructor, Nil, List(Bind(elem1, Wildcard()), Bind(elem2, Wildcard())))

    val destruct = Unapply(destructor, Nil, List(innerDestruct, Bind(elem3, Wildcard())))

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
