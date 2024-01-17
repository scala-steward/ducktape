package io.github.arainko.ducktape.internal

import io.github.arainko.ducktape.Transformer

import scala.annotation.*
import scala.quoted.*
import scala.util.chaining.*
import io.github.arainko.ducktape.Mode

private[ducktape] object ProductBinder {

  def nestFlatMapsAndConstruct[F[+x]: Type, Dest: Type](
    F: Expr[Mode.FailFast[F]],
    fields: List[ProductZipper.Field.Wrapped[F] | ProductZipper.Field.Unwrapped],
    construct: List[ProductZipper.Field.Unwrapped] => Expr[Dest]
  )(using Quotes): Expr[F[Dest]] = {
    def recurse(
      leftoverFields: List[ProductZipper.Field.Wrapped[F] | ProductZipper.Field.Unwrapped],
      collectedUnwrappedFields: List[ProductZipper.Field.Unwrapped]
    )(using Quotes): Expr[F[Dest]] =
      leftoverFields match {
        case ProductZipper.Field.Wrapped(field, value) :: Nil =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $F.map[`destField`, Dest](
                  $value,
                  ${
                    generateLambda[[A] =>> A, destField, Dest](
                      field,
                      unwrappedValue =>
                        construct(ProductZipper.Field.Unwrapped(field, unwrappedValue) :: collectedUnwrappedFields)
                    )
                  }
                )
              }
          }

        case ProductZipper.Field.Wrapped(field, value) :: next =>
          value match {
            case '{ $value: F[destField] } =>
              '{
                $F.flatMap[`destField`, Dest](
                  $value,
                  ${
                    generateLambda[F, destField, Dest](
                      field,
                      unwrappedValue =>
                        recurse(next, ProductZipper.Field.Unwrapped(field, unwrappedValue) :: collectedUnwrappedFields)
                    )
                  }
                )
              }
          }

        case (f: ProductZipper.Field.Unwrapped) :: next =>
          recurse(next, f :: collectedUnwrappedFields)

        case Nil =>
          val constructedValue = construct(collectedUnwrappedFields)
          '{ $F.pure[Dest]($constructedValue) }
      }

    recurse(fields, Nil)
  }

  // this fixes a weird compiler crash where if I use the same name for each of the lambda args the compiler is not able to find a proxy for one of the invocations (?)
  // this probably warrants a crash report?
  @nowarn // todo: use @unchecked?
  private def generateLambda[F[+x]: Type, A: Type, B: Type](field: ProductZipper.Field, f: Expr[A] => Expr[F[B]])(using
    Quotes
  ) = {
    import quotes.reflect.*

    val mtpe = MethodType(List(field.name))(_ => List(TypeRepr.of[A]), _ => TypeRepr.of[F[B]])
    Lambda(
      Symbol.spliceOwner,
      mtpe,
      { case (methSym, (arg1: Term) :: Nil) => f(arg1.asExprOf[A]).asTerm.changeOwner(methSym) }
    ).asExprOf[A => F[B]]
  }
}
