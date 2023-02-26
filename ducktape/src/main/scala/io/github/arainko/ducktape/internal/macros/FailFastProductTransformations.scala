package io.github.arainko.ducktape.internal.macros

import io.github.arainko.ducktape.internal.modules.*
import io.github.arainko.ducktape.partial.FailFast

import scala.annotation.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.util.chaining.*

object FailFastProductTransformations {
  def transform[F[+x]: Type, Source: Type, Dest: Type](
    Source: Expr[Mirror.ProductOf[Source]],
    Dest: Expr[Mirror.ProductOf[Dest]],
    F: Expr[FailFast.Support[F]],
    sourceValue: Expr[Source]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    given Fields.Source = Fields.Source.fromMirror(Source)
    given Fields.Dest = Fields.Dest.fromMirror(Dest)

    createTransformation[F, Source, Dest](F, sourceValue, Fields.dest.value)
  }

  inline def transformFailFast[F[+x], Source, Dest](
    sourceValue: Source
  )(using F: FailFast.Support[F], inline Source: Mirror.ProductOf[Source], inline Dest: Mirror.ProductOf[Dest]) =
    ${ transform[F, Source, Dest]('Source, 'Dest, 'F, 'sourceValue) }

  private def createTransformation[F[+x]: Type, Source: Type, Dest: Type](
    F: Expr[FailFast.Support[F]],
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

        source.partialTransformerTo[F, FailFast](dest).asExpr match {
          case '{ FailFast.partialFromTotal[F, src, dest](using $total, $support) } =>
            val sourceField = sourceValue.accessField(source).asExprOf[src]
            val lifted = LiftTransformation.liftTransformation[src, dest](total, sourceField)
            Field.Unwrapped(dest, lifted)
          case '{ $transformer: FailFast[F, src, dest] } =>
            val sourceField = sourceValue.accessField(source).asExprOf[src]
            Field.Wrapped(dest, '{ $transformer.transform($sourceField) })
        }
      }

    nestFlatMapsAndConstruct[F, Dest](F, transformedFields)
  }

  private def nestFlatMapsAndConstruct[F[+x]: Type, Dest: Type](
    F: Expr[FailFast.Support[F]],
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
                $F.map[`destField`, Dest](
                  $value,
                  ${
                    generateLambda[[A] =>> A, destField, Dest](
                      field,
                      unwrappedValue =>
                        Constructor.construct[Dest](Field.Unwrapped(field, unwrappedValue) :: collectedUnwrappedFields)
                    )
                  }
                )
              }
          }

        case Field.Wrapped(field, value) :: next =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $F.flatMap[`destField`, Dest](
                  $value,
                  ${
                    generateLambda[F, destField, Dest](
                      field,
                      unwrappedValue => recurse(next, Field.Unwrapped(field, unwrappedValue) :: collectedUnwrappedFields)
                    )
                  }
                )
              }
          }

        case (f: Field.Unwrapped) :: next =>
          recurse(next, f :: collectedUnwrappedFields)

        case Nil =>
          val constructedValue = Constructor.construct[Dest](collectedUnwrappedFields)
          '{ $F.pure[Dest]($constructedValue) }
      }

    recurse(fields, Nil)
  }

  // this fixes a weird compiler crash where if I use the same name for each of the lambda args the compiler is not able to find a proxy for one of the invocations (?)
  // this probably warrants a crash report?
  @nowarn // todo: use @unchecked?
  private def generateLambda[F[+x]: Type, A: Type, B: Type](field: Field, f: Expr[A] => Expr[F[B]])(using Quotes) = {
    import quotes.reflect.*

    val mtpe = MethodType(List(field.name))(_ => List(TypeRepr.of[A]), _ => TypeRepr.of[F[B]])
    Lambda(
      Symbol.spliceOwner,
      mtpe,
      { case (methSym, (arg1: Term) :: Nil) => f(arg1.asExprOf[A]).asTerm.changeOwner(methSym) }
    ).asExprOf[A => F[B]]
  }
}
