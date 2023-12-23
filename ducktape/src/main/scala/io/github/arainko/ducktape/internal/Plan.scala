package io.github.arainko.ducktape.internal

import io.github.arainko.ducktape.*
import io.github.arainko.ducktape.internal.*

import scala.collection.immutable.ListMap
import scala.quoted.*

private[ducktape] sealed trait Plan[+E <: Plan.Error] {
  import Plan.*

  def source: Structure

  def dest: Structure

  // final def sourceTpe: Type[?] = source.tpe

  // final def destTpe: Type[?] = dest.tpe

  def sourceContext: Path

  def destContext: Path

  final def configureAll(configs: List[Configuration.At])(using Quotes): Plan.Reconfigured = PlanConfigurer.run(this, configs)

  final def refine: Either[NonEmptyList[Plan.Error], Plan[Nothing]] = PlanRefiner.run(this)
}

private[ducktape] object Plan {
  case class Upcast(
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path
  ) extends Plan[Nothing]

  case class UserDefined(
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    transformer: Expr[Transformer[?, ?]]
  ) extends Plan[Nothing]

  case class Derived(
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    transformer: Expr[Transformer.Derived[?, ?]]
  ) extends Plan[Nothing]

  case class Configured(
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    config: Configuration
  ) extends Plan[Nothing]

  case class BetweenProductFunction[+E <: Plan.Error](
    source: Structure.Product,
    dest: Structure.Function,
    sourceContext: Path,
    destContext: Path,
    argPlans: ListMap[String, Plan[E]]
  ) extends Plan[E]

  case class BetweenUnwrappedWrapped(
    source: Structure,
    dest: Structure.ValueClass,
    sourceContext: Path,
    destContext: Path
  ) extends Plan[Nothing]

  case class BetweenWrappedUnwrapped(
    source: Structure.ValueClass,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    fieldName: String
  ) extends Plan[Nothing]

  case class BetweenSingletons(
    source: Structure.Singleton,
    dest: Structure.Singleton,
    sourceContext: Path,
    destContext: Path
  ) extends Plan[Nothing]

  case class BetweenProducts[+E <: Plan.Error](
    source: Structure.Product,
    dest: Structure.Product,
    sourceContext: Path,
    destContext: Path,
    fieldPlans: Map[String, Plan[E]]
  ) extends Plan[E]

  case class BetweenCoproducts[+E <: Plan.Error](
    source: Structure.Coproduct,
    dest: Structure.Coproduct,
    sourceContext: Path,
    destContext: Path,
    casePlans: Vector[Plan[E]]
  ) extends Plan[E]

  case class BetweenOptions[+E <: Plan.Error](
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    plan: Plan[E]
  ) extends Plan[E]

  case class BetweenNonOptionOption[+E <: Plan.Error](
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    plan: Plan[E]
  ) extends Plan[E]

  case class BetweenCollections[+E <: Plan.Error](
    destCollectionTpe: Type[? <: Iterable[?]],
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    plan: Plan[E]
  ) extends Plan[E]

  case class Error(
    source: Structure,
    dest: Structure,
    sourceContext: Path,
    destContext: Path,
    message: ErrorMessage,
    suppressed: Option[Plan.Error]
  ) extends Plan[Plan.Error]

  object Error {
    def from(plan: Plan[Plan.Error], message: ErrorMessage, suppressed: Option[Plan.Error]): Plan.Error =
      Plan.Error(
        plan.source,
        plan.dest,
        plan.sourceContext,
        plan.destContext,
        message,
        suppressed
      )
  }

  def unapply[E <: Plan.Error](plan: Plan[E]): (Type[?], Type[?]) = (plan.source.tpe, plan.dest.tpe)

  given debug[E <: Plan.Error]: Debug[Plan[E]] = Debug.derived

  final case class Reconfigured(
    errors: List[Plan.Error],
    successes: List[Configuration.At.Successful],
    result: Plan[Plan.Error]
  ) derives Debug
}
