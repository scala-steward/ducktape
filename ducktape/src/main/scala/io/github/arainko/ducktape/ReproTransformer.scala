package io.github.arainko.ducktape

import scala.quoted.*

// trait ReproTransformer[A, B] {
//   def transform(value: A): B
// }

// object ReproTransformer {
//   given identity[A]: ReproTransformer[A, A] with {
//     def transform(value: A): A = value
//   }

//   inline given toAnyVal[A, B <: AnyVal]: ReproTransformer[A, B] = ${ toAnyValTransformer[A, B] }

//   inline def summon[A, B]: ReproTransformer[A, B] = ${ summonTransformer[A, B] }

//   private def summonTransformer[A: Type, B: Type](using Quotes) = Expr.summon[ReproTransformer[A, B]].get

//   private def toAnyValTransformer[A: Type, B <: AnyVal: Type](using Quotes): Expr[ReproTransformer[A, B]] = 
//     '{ value => ${ transform[A, B]('value) } }
  
//   private def transform[Source: Type, Dest <: AnyVal: Type](source: Expr[Source])(using Quotes): Expr[Dest] = {
//     import quotes.reflect.*

//     val tpe = TypeRepr.of[Dest]

//     println(s"Evaluating 'transform' with Source: ${Type.show[Source]} and Dest: ${Type.show[Dest]}")

//     New(Inferred(tpe))
//       .select(tpe.typeSymbol.primaryConstructor)
//       .appliedTo(source.asTerm)
//       .asExprOf[Dest]
//   }
// }

trait ReproTransformer[A, B] {
  def transform(value: A): B
}

object ReproTransformer {
  given identity[A]: ReproTransformer[A, A] with {
    def transform(value: A): A = value
  }

  inline given toAnyVal[A, B](using B <:< AnyVal): ReproTransformer[A, B] = ${ toAnyValTransformer[A, B] }

  inline def summon[A, B]: ReproTransformer[A, B] = ${ summonTransformer[A, B] }

  private def summonTransformer[A: Type, B: Type](using Quotes) = Expr.summon[ReproTransformer[A, B]].get

  private def toAnyValTransformer[A: Type, B: Type](using Quotes): Expr[ReproTransformer[A, B]] = 
    '{ value => ${ transform[A, B]('value) } }
  
  private def transform[Source: Type, Dest: Type](source: Expr[Source])(using Quotes): Expr[Dest] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[Dest]

    println(s"Evaluating 'transform' with Source: ${Type.show[Source]} and Dest: ${Type.show[Dest]}")

    New(Inferred(tpe))
      .select(tpe.typeSymbol.primaryConstructor)
      .appliedTo(source.asTerm)
      .asExprOf[Dest]
  }
}
