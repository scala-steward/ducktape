package io.github.arainko.ducktape.internal.modules

import scala.quoted.*
import io.github.arainko.ducktape.internal.modules.Field.Unwrapped

object ZippedProduct {

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
  def unzip(nestedPairs: Expr[Any], fields: List[Field])(using Quotes) = {
    import quotes.reflect.*

    def recurse(
      tpe: Type[?],
      leftoverFields: List[Field]
      )(using Quotes): Option[(quotes.reflect.Unapply | quotes.reflect.Bind, List[Field.Unwrapped])] = {
      import quotes.reflect.*

      (tpe -> leftoverFields) match {
        case ('[Tuple2[first, second]], firstField :: secondField :: Nil) =>
          val firstTpe = TypeRepr.of[second]
          val secondTpe = TypeRepr.of[first]
          val firstBind = Symbol.newBind(Symbol.spliceOwner, firstField.name, Flags.Local, firstTpe)
          val secondBind = Symbol.newBind(Symbol.spliceOwner, secondField.name, Flags.Local, secondTpe)
          val fields =
            List(Field.Unwrapped(secondField, Ref(secondBind).asExpr), Field.Unwrapped(firstField, Ref(firstBind).asExpr))
          val extractor =
            Unapply(Tuple2Extractor(secondTpe, firstTpe), Nil, Bind(secondBind, Wildcard()) :: Bind(firstBind, Wildcard()) :: Nil)
          Some(extractor -> fields)

        case ('[tpe], field :: Nil) => 
          val tpe = TypeRepr.of(using field.tpe)
          val bind = Symbol.newBind(Symbol.spliceOwner, field.name, Flags.Local, tpe)
          Some(Bind(bind, Wildcard()) -> (Field.Unwrapped(field, Ref(bind).asExpr) :: Nil))

        case ('[Tuple2[rest, current]], field :: tail) =>
          val restTpe = TypeRepr.of[rest]
          val currentTpe = TypeRepr.of[current]
          val pairExtractor = Tuple2Extractor(restTpe, currentTpe)
          val bind = Symbol.newBind(Symbol.spliceOwner, field.name, Flags.Local, currentTpe)
          recurse(summon[Type[rest]], tail).map { (pattern, unzippedFields) =>
            val extractor = Unapply(pairExtractor, Nil, pattern :: Bind(bind, Wildcard()) :: Nil)
            val fields = Field.Unwrapped(field, Ref(bind).asExpr) :: unzippedFields
            extractor -> fields
          }


        case _ => None
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

/*
def recurse(
      fields: List[Field]
      // collectedFields: List[UnwrappedField]
    )(using Quotes): Option[(Unapply | Bind, List[UnwrappedField])] =
      fields match {
        case first :: second :: Nil => // Unapply with two binds
          val firstTpe = TypeRepr.of(using first.tpe)
          val secondTpe = TypeRepr.of(using second.tpe)
          val firstBind = Symbol.newBind(Symbol.spliceOwner, first.name, Flags.Local, firstTpe)
          val secondBind = Symbol.newBind(Symbol.spliceOwner, second.name, Flags.Local, secondTpe)
          val fields =
            UnwrappedField(first.name, Ref(firstBind).asExpr) :: UnwrappedField(second.name, Ref(secondBind).asExpr) :: Nil
          val extractor = Unapply(Tuple2Extractor.appliedToTypes(secondTpe :: firstTpe :: Nil), Nil, Bind(secondBind, Wildcard()) :: Bind(firstBind, Wildcard()) :: Nil)
          Some(extractor -> fields)

        case single :: Nil =>
          val bind = Symbol.newBind(Symbol.spliceOwner, single.name, Flags.Local, TypeRepr.of(using single.tpe))
          Some(Bind(bind, Wildcard()) -> (UnwrappedField(single.name, Ref(bind).asExpr) :: Nil))

        case single :: next =>
          val bind = Symbol.newBind(Symbol.spliceOwner, single.name, Flags.Local, TypeRepr.of(using single.tpe))
          recurse(next).map { (pattern, collectedFields) =>
            val extractor = Unapply(Tuple2Extractor, Nil, pattern :: Bind(bind, Wildcard()) :: Nil)
            val fields = UnwrappedField(single.name, Ref(bind).asExpr) :: collectedFields
            extractor -> fields
          }

        case Nil => None
      }
 */
