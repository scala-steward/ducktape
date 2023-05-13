package io.github.arainko.ducktape.internal.modules

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*
import io.github.arainko.ducktape.internal.util.*

type TpeRepr = (quotes: Quotes) ?=> quotes.reflect.TypeRepr
type TpeReprs = (quotes: Quotes) ?=> List[quotes.reflect.TypeRepr]
type Label = (quotes: Quotes) ?=> String
type Labels = (quotes: Quotes) ?=> List[String]

private[ducktape] final class MaterializedMirror(
  val mirroredType: TpeRepr,
  val mirroredElemTypes: TpeReprs,
  val mirroredLabel: Label,
  val mirroredElemLabels: Labels
)

// Lifted from shapeless 3:
// https://github.com/typelevel/shapeless-3/blob/main/modules/deriving/src/main/scala/shapeless3/deriving/internals/reflectionutils.scala
private[ducktape] object MaterializedMirror {

  def createOrAbort[A: Type](mirror: Expr[Mirror.Of[A]])(using Quotes): MaterializedMirror = 
    mirror match {
      case '{
            $m: Mirror.Of[A] {
              type MirroredElemLabels = labels
              type MirroredElemTypes = types
              type MirroredLabel = label
            }
          } => 
        val m = MaterializedMirror(
          mirroredType = quotes ?=> quotes.reflect.TypeRepr.of[A],
          mirroredElemTypes = quotes ?=> tupleTypeElements(quotes.reflect.TypeRepr.of[types]),
          mirroredLabel = 
            quotes ?=> {
            import quotes.reflect.*
            val ConstantType(StringConstant(tpeLabel)) = TypeRepr.of[label]: @unchecked
            tpeLabel
          },
          mirroredElemLabels = 
            quotes ?=> {
            import quotes.reflect.*
            tupleTypeElements(TypeRepr.of[labels]).map { case ConstantType(StringConstant(l)) => l }
          }
        )
        m
    }

  def costam = {
    val m: MaterializedMirror = ???
    val mapped: (quotes: Quotes) ?=> String = quotes ?=> m.mirroredElemLabels.mkString
  }

  // def createOrAbort[A: Type](mirror: Expr[Mirror.Of[A]])(using Quotes): MaterializedMirror[quotes.type] =
  //   create(mirror).fold(memberName => Failure.emit(Failure.MirrorMaterialization(summon, memberName)), identity)

  // private def create(mirror: Expr[Mirror])(using Quotes): Either[String, MaterializedMirror[quotes.type]] = {
  //   import quotes.reflect.*

  //   val mirrorTpe = mirror.asTerm.tpe.widen
  //   for {
  //     mirroredType <- findMemberType(mirrorTpe, "MirroredType")
  //     mirroredMonoType <- findMemberType(mirrorTpe, "MirroredMonoType")
  //     mirroredElemTypes <- findMemberType(mirrorTpe, "MirroredElemTypes")
  //     mirroredLabel <- findMemberType(mirrorTpe, "MirroredLabel")
  //     mirroredElemLabels <- findMemberType(mirrorTpe, "MirroredElemLabels")
  //   } yield {
  //     val elemTypes = tupleTypeElements(mirroredElemTypes)
  //     val ConstantType(StringConstant(label)) = mirroredLabel: @unchecked
  //     val elemLabels = tupleTypeElements(mirroredElemLabels).map { case ConstantType(StringConstant(l)) => l }
  //     MaterializedMirror(mirroredType, mirroredMonoType, elemTypes, label, elemLabels)
  //   }
  // }

  private def tupleTypeElements(using Quotes)(tp: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*

    @tailrec def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tp match {
      case AppliedType(pairTpe, List(hd: TypeRepr, tl: TypeRepr)) => loop(tl, hd :: acc)
      case _                                                      => acc
    }
    loop(tp, Nil).reverse
  }

  private def low(using Quotes)(tp: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*

    tp match {
      case tp: TypeBounds => tp.low
      case tp             => tp
    }
  }

  private def findMemberType(using Quotes)(tp: quotes.reflect.TypeRepr, name: String): Either[String, quotes.reflect.TypeRepr] = {
    import quotes.reflect.*

    tp match {
      case Refinement(_, `name`, tp) => Right(low(tp))
      case Refinement(parent, _, _)  => findMemberType(parent, name)
      case AndType(left, right)      => findMemberType(left, name).orElse(findMemberType(right, name))
      case _                         => Left(name)
    }
  }

}
