package io.github.arainko.ducktape.internal.modules

import scala.quoted.*
import io.github.arainko.ducktape.internal.modules.Field.Unwrapped
import io.github.arainko.ducktape.internal.macros.*
import scala.annotation.tailrec

object ZippedProduct {

  val cos = 1 -> 1d -> 1f -> "asd"

  val str = cos._2
  val float = cos._1._2
  val double = DebugMacros.structure(cos._1._1._2)
  val int = cos._1._1._1

  private enum TupleField {
    case First, Second
  }

  private def access(using Quotes)(term: quotes.reflect.Term, tupleField: TupleField) = {
    import quotes.reflect.*
    val field = tupleField match {
      case TupleField.First  => "_1"
      case TupleField.Second => "_2"
    }
    Select.unique(term, field)
  }

  @tailrec
  private def accessN(using Quotes)(term: quotes.reflect.Term, times: Int): quotes.reflect.Term =
    times match {
      case 0    => term
      case next => accessN(access(term, TupleField.First), next - 1)
    }

  def unzip2(
    nestedPairs: Expr[Any],
    fields: ::[Field.Wrapped[?]]
  )(using Quotes): List[Field.Unwrapped] = {
    import quotes.reflect.*

    val fs = fields.reverse.zipWithIndex.reverse
    val last = fields.size - 1

    def recurse(leftoverFields: List[(Field.Wrapped[?], Int)], acc: List[Field.Unwrapped])(using
      Quotes
    ): List[Field.Unwrapped] = {
      import quotes.reflect.*

      leftoverFields match {
        case (Field.Wrapped(field, _), idx) :: Nil => ???

        case (f1, idx1) :: (f2, idx2) :: Nil => ???
        case (f, idx) :: next                => ???
        case Nil                             => acc
      }
    }

    /*
    val tup = Tuple2[Tuple2[Tuple2[Int, Double], Float], String]
    String =  ._2
    Float =   ._1._2
    Double =  ._1._1._2
    Int =     ._1._1._1
     */

    ???
  }

  /**
   * Imagine you have a value of type: Tuple2[Tuple2[Tuple2[Int, Double], Float], String], eg.
   *     val whatever: Tuple2[Tuple2[Tuple2[Int, Double], Float], String] = 1 -> 1d -> 1f -> "4"
   *
   * and fields that were 'zipped' into this shape with `PartialTransformer.Accumulating.Support#product`
   * by applying this operation left to right eg.
   *     (field1: Int).product(field2: Double).product(field3: Float)
   *
   * The fields need to be provided in THE EXACT SAME ORDER they were zipped so for the operation above we'd expect
   *    List(field1, field2, field3, field4).
   *
   * This method will generate a tree that unpacks such an associated tuple, eg. for the example above we'd get a tree that corresponds to
   * a pattern match:
   *      case Tuple2(Tuple2(Tuple(field1, field2), field3), field4)
   *
   * and a list of unwrapped fields that allow you to operate on the bound values of the pattern match.
   */
  def unzip(
    nestedPairs: Expr[Any],
    fields: ::[Field.Wrapped[?]]
  )(using Quotes): (quotes.reflect.Unapply | quotes.reflect.Bind, List[Unwrapped]) = {
    import quotes.reflect.*

    def recurse(
      tpe: Type[?],
      leftoverFields: List[Field.Wrapped[?]]
    )(using Quotes): (quotes.reflect.Unapply | quotes.reflect.Bind, List[Field.Unwrapped]) = {
      import quotes.reflect.*

      (tpe -> leftoverFields) match {
        case ('[Tuple2[first, second]], Field.Wrapped(firstField, _) :: Field.Wrapped(secondField, _) :: Nil) =>
          val firstTpe = TypeRepr.of[second]
          val secondTpe = TypeRepr.of[first]
          val firstBind = Symbol.newBind(Symbol.spliceOwner, firstField.name, Flags.Local, firstTpe)
          val secondBind = Symbol.newBind(Symbol.spliceOwner, secondField.name, Flags.Local, secondTpe)
          val fields =
            List(Field.Unwrapped(secondField, Ref(secondBind).asExpr), Field.Unwrapped(firstField, Ref(firstBind).asExpr))
          val extractor =
            Unapply(Tuple2Extractor(secondTpe, firstTpe), Nil, Bind(secondBind, Wildcard()) :: Bind(firstBind, Wildcard()) :: Nil)
          extractor -> fields

        case ('[tpe], Field.Wrapped(field, _) :: Nil) =>
          val tpe = TypeRepr.of(using field.tpe)
          val bind = Symbol.newBind(Symbol.spliceOwner, field.name, Flags.Local, tpe)
          Bind(bind, Wildcard()) -> (Field.Unwrapped(field, Ref(bind).asExpr) :: Nil)

        case ('[Tuple2[rest, current]], Field.Wrapped(field, _) :: tail) =>
          val restTpe = TypeRepr.of[rest]
          val currentTpe = TypeRepr.of[current]
          val pairExtractor = Tuple2Extractor(restTpe, currentTpe)
          val bind = Symbol.newBind(Symbol.spliceOwner, field.name, Flags.Local, currentTpe)
          val (pattern, unzippedFields) = recurse(summon[Type[rest]], tail)
          val extractor = Unapply(pairExtractor, Nil, pattern :: Bind(bind, Wildcard()) :: Nil)
          val fields = Field.Unwrapped(field, Ref(bind).asExpr) :: unzippedFields
          extractor -> fields

        case (tpe, fields) =>
          val printedType = TypeRepr.of(using tpe).show
          report.errorAndAbort(s"Unexpected state reached while unzipping a product, tpe: $printedType, fields: ${fields}")

      }
    }

    recurse(nestedPairs.asTerm.tpe.asType, fields.reverse)
  }

  private def Tuple2Extractor(using Quotes)(A: quotes.reflect.TypeRepr, B: quotes.reflect.TypeRepr) = {
    import quotes.reflect.*

    Select
      .unique('{ Tuple2 }.asTerm, "unapply")
      .appliedToTypes(A :: B :: Nil)
  }

}
