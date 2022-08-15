package io.github.arainko.ducktape.internal.modules

import scala.quoted.*
import scala.deriving.*
import io.github.arainko.ducktape.Transformer
import scala.deriving.*
import io.github.arainko.ducktape.Arg

private[internal] trait Module {
  val quotes: Quotes

  given Quotes = quotes

  import quotes.reflect.*

  given Printer[TypeRepr] = Printer.TypeReprShortCode
  given Printer[Tree] = Printer.TreeShortCode

  def mirrorOf[A: Type]: Option[Expr[Mirror.Of[A]]] = Expr.summon[Mirror.Of[A]]

  def abort(error: Failure): Nothing =
    report.errorAndAbort(error.render, error.position)

  opaque type Suggestion = String

  object Suggestion {
    def apply(text: String): Suggestion = text

    def all(head: String, tail: String*): List[Suggestion] = head :: tail.toList

    /**
     * Prepends a newline, adds a '|' (to work with .stripPrefix) and a bullet point character to each suggestion.
     */
    def renderAll(suggestions: List[Suggestion]): String =
      suggestions.mkString("\n| • ", "\n| • ", "")
  }

  sealed trait Failure {
    def position: Position = Position.ofMacroExpansion

    def render: String
  }

  object Failure {
    final case class MirrorMaterialization(mirroredType: TypeRepr, notFoundTypeMemberName: String) extends Failure {

      def render: String =
        s"""
        |Mirror materialization for ${mirroredType.show} failed. 
        |Member type not found: '$notFoundTypeMemberName'.
        """.stripMargin
    }

    final case class InvalidFieldSelector(
      selector: Expr[Any],
      sourceTpe: TypeRepr,
      suggestedFields: List[Suggestion]
    ) extends Failure {
      override def position = selector.asTerm.pos

      def render: String =
        s"""
        |'${selector.asTerm.show}' is not a valid field selector for ${sourceTpe.show}.
        |Try one of these: ${Suggestion.renderAll(suggestedFields)}
        """.stripMargin
    }

    enum InvalidArgSelector extends Failure {
      override def position: Position =
        this match {
          case NotFound(selector, _, _)                  => selector.asTerm.pos
          case TypeMismatch(_, _, _, _, mismatchedValue) => mismatchedValue.asTerm.pos
          case NotAnArgSelector(selector, _)             => selector.asTerm.pos
        }

      final def render = this match {
        case NotFound(_, argName, suggestedArgs) =>
          s"""
            |'_.$argName' is not a valid argument selector.
            |Try one of these: ${Suggestion.renderAll(suggestedArgs)}
        """.stripMargin
        case TypeMismatch(_, argName, expectedType, actualTpe, _) =>
          s"""
              |Type mistmatch for argument '$argName'.
              |Expected ${expectedType.show} but found ${actualTpe.show}.
        """.stripMargin
        case NotAnArgSelector(_, suggestedArgs) =>
          s"""
              |Not a valid argument selector.
              |Try one of these: ${Suggestion.renderAll(suggestedArgs)}
        """.stripMargin
      }

      case NotFound(selector: Expr[Any], argumentName: String, suggestedArgs: List[Suggestion])

      case TypeMismatch(
        selector: Expr[Any],
        argumentName: String,
        expectedType: TypeRepr,
        actualType: TypeRepr,
        mismatchedValue: Expr[Any]
      )

      case NotAnArgSelector(selector: Expr[Any], suggestedArgs: List[Suggestion])
    }

    final case class UnsupportedConfig(config: Expr[Any], configFor: "arg" | "field" | "case") extends Failure {
      private def fieldOrArgSuggestions(fieldOrArg: "arg" | "field") = {
        val capitalized = fieldOrArg.capitalize
        Suggestion.all(
          s"""${capitalized}.const(_.${fieldOrArg}Name, "value")""",
          s"""${capitalized}.computed(_.${fieldOrArg}Name, source => source.value)""",
          s"""${capitalized}.renamed(_.${fieldOrArg}Name1, _.${fieldOrArg}Name2)"""
        )
      }

      private val caseSuggestions = Suggestion.all(
        """Case.const[SourceSubtype.type](SourceSubtype.value)""",
        """Case.computed[SourceSubtype.type](source => source.value)"""
      )

      private val suggestions =
        configFor match {
          case arg: "arg"     => fieldOrArgSuggestions(arg)
          case field: "field" => fieldOrArgSuggestions(field)
          case coprod: "case" => caseSuggestions
        }

      override def position = config.asTerm.pos

      def render: String =
        s"""
        |'${config.asTerm.show}' is not a supported $configFor configuration expression.
        |Try one of these: ${Suggestion.renderAll(suggestions)}
        |
        |Please note that you HAVE to use these directly as variadic arguments (not through a proxy method,
        |not with the splash operator (eg. Seq()*) etc.).
        """.stripMargin
    }

  }

}
