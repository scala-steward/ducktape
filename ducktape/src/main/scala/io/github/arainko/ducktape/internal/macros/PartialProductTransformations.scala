package io.github.arainko.ducktape.internal.macros

import scala.quoted.*
import io.github.arainko.ducktape.PartialTransformer
import io.github.arainko.ducktape.internal.modules.*
import scala.deriving.Mirror

object PartialProductTransformations {
  def transformFailFast[F[+x]: Type, Source: Type, Dest: Type](
    Source: Expr[Mirror.ProductOf[Source]],
    Dest: Expr[Mirror.ProductOf[Dest]],
    support: Expr[PartialTransformer.FailFast.Support[F]]
  )(using Quotes): Expr[F[Dest]] = {
    import quotes.reflect.*

    given Fields.Source = Fields.Source.fromMirror(Source)
    given Fields.Dest = Fields.Dest.fromMirror(Dest)

    ???
  }

  def nestedFlatMaps(exprs: List[Expr[Option[Int]]], collectedValues: List[Expr[Int]])(using Quotes): Expr[Option[List[Int]]] = {
    import quotes.reflect.*

    // Tuple2.unapply()

    exprs match {
      case head :: next => 
        '{ $head.flatMap(a => ${nestedFlatMaps(next, 'a :: collectedValues)}) }
      case Nil => '{ Some(${Varargs(collectedValues.reverse)}.toList) }
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
          destruct, None, Ref(elem1)
        )
      )
    ).asExprOf[Int]
  }

  inline def unnestUsage(value: ((Int, Int), Int)) = ${ unnestTuple('value) }

  // private def fieldTransformations[F[+x]: Type, Source: Type](
  //   support: Expr[PartialTransformer.FailFast.Support[F]],
  //   sourceValue: Expr[Source],
  //   fieldsToTransformInto: List[Field]
  // )(using Quotes, Fields.Source) = {
  //   import quotes.reflect.*

  //   fieldsToTransformInto.map { field =>
  //     field ->
  //       Fields.source
  //         .get(field.name)
  //         .getOrElse(Failure.abort(Failure.NoFieldMapping(field.name, Type.of[Source])))
  //   }.map { (dest, source) =>
  //     val call = resolveTransformation(sourceValue, source, dest)

  //     NamedArg(dest.name, call)
  //   }
  // }

  private def accessField(value: Expr[Any], fieldName: String)(using Quotes) = {
    import quotes.reflect.*

    Select.unique(value.asTerm, fieldName)
  }
}
