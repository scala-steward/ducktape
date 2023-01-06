package io.github.arainko.ducktape

import scala.quoted.*
import scala.deriving.Mirror

@FunctionalInterface
trait ReproTransformer[A, B] {
  def transform(from: A): B
}

object ReproTransformer {
  final class Identity[A, B >: A] extends ReproTransformer[A, B] {
    def transform(from: A): B = from
  }

  given identity[A, B >: A]: Identity[A, B] = Identity[A, B]

  inline given derived[A <: Product, B <: Product](using Mirror.ProductOf[A], Mirror.ProductOf[B]): ReproTransformer[A, B] = 
    ${ deriveProductTransformerMacro[A, B] }

  def deriveProductTransformerMacro[A: Type, B: Type](using Quotes): Expr[ReproTransformer[A, B]] =
    '{ value => ${ transformProductMacro[A, B]('value) } }

  def transformProductMacro[A: Type, B: Type](source: Expr[A])(using Quotes): Expr[B] = {
    import quotes.reflect.*

    val sourceTpe = TypeRepr.of[A]
    val destTpe = TypeRepr.of[B]

    def fields(tpe: TypeRepr) =
      tpe.typeSymbol.caseFields
        .map(sym => sym.name -> tpe.memberType(sym))
        .toMap

    def accessField(source: Expr[A], name: String) = Select.unique(source.asTerm, name)

    val sourceFields = fields(sourceTpe)
    val destFields = fields(destTpe)

    val fieldTransformations =
      sourceFields.map { (name, sourceTpe) =>
        val destTpe = destFields(name)
        (sourceTpe.asType -> destTpe.asType) match {
          case '[src] -> '[dest] =>
            val transformer = Expr.summon[ReproTransformer[src, dest]].getOrElse(report.errorAndAbort(s"Not found for $name"))
            val field = accessField(source, name).asExprOf[src]
            NamedArg(name, '{ $transformer.transform($field) }.asTerm)
        }
      }.toList

    val constructorSym = destTpe.typeSymbol.primaryConstructor
    New(Inferred(destTpe)).select(constructorSym).appliedToArgs(fieldTransformations).asExprOf[B]
  }
}
