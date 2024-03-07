package io.github.arainko.ducktape.docs

import org.scalafmt.dynamic.ConsoleScalafmtReporter
import org.scalafmt.interfaces.Scalafmt

import java.io.{ OutputStream, PrintStream }
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.stream.Collectors
import scala.quoted.*
import scala.util.chaining.*

/**
 * Sometimes the code printed with `Printer.TreeShortCode` is not fully legal (at least in scalafmt terms)
 * so we create an error reporter that doesn't report anything to not bother ourselves with weird stacktraces when the docs are compiling
 */
object SilentReporter extends ConsoleScalafmtReporter(PrintStream(OutputStream.nullOutputStream()))

object Docs {
  val scalafmt = Scalafmt.create(this.getClass.getClassLoader()).withReporter(SilentReporter)
  val config = Path.of(".scalafmt.conf")

  inline def printCode[A](inline value: A): A = ${ printCodeMacro[A]('value) }

  def printCodeMacro[A: Type](value: Expr[A])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    /*
     * Printer.TreeShortCode prints type lambdas with a => instead of =>> which makes scalafmt REALLY, REALLY upset
     * this beauty fixes it until https://github.com/lampepfl/dotty/pull/16968 is merged to dotty
     */
    val typeLambdaSingleArgRegex = """\[(\[\p{L}+ >: Nothing <: \p{L}+\]) =>""".r
    val typeLambdaTwoArgsRegex = """\[(\[\p{L}+ >: Nothing <: \p{L}+\, \p{L}+ >: Nothing <: \p{L}+\]) =>""".r
    def fixTypeLambdas(input: String): String =
      typeLambdaSingleArgRegex
        .replaceAllIn(input, "[$1 =>>")
        .pipe(typeLambdaTwoArgsRegex.replaceAllIn(_, "[$1 =>>"))

    val struct = fixTypeLambdas(Printer.TreeShortCode.show(value.asTerm))

    // we need to enclose it inside a class/object, otherwise scalafmt doesn't run
    val enclosedCode =
      s"""object Code {
      |  $struct
      |}""".stripMargin

    val formatted =
      scalafmt
        .format(config, Path.of("Code.scala"), enclosedCode)
        .linesWithSeparators
        .toVector
        .drop(1) // strip 'object Code {'
        .dropRight(1) // strip the block-closing '}'
        .prepended("``` scala \n") // enclose it in a Scala markdown block
        .appended("```")
        .mkString

    '{
      println(${ Expr(formatted) })
      $value
    }
  }

  def generateTableOfContents() = {
    enum Content {
      def value: String

      final def render =
        this match
          case Header(value)    => s"* [$value](${headerName(value)})"
          case Subheader(value) => s"  * [$value](${headerName(value)})"

      case Header(value: String)
      case Subheader(value: String)
    }

    object Content {
      def fromLine(line: String) =
        PartialFunction.condOpt(line) {
          case s"## $contents"   => Content.Header(contents)
          case s"### $contents"  => Content.Subheader(contents)
          case s"#### $contents" => Content.Subheader(contents)
        }
    }

    def headerName(string: String) =
      "#" +
        string.trim
          .replace(' ', '-')
          .filter(char => char.isLetterOrDigit || char == '-')
          .toLowerCase

    val tableOfContents =
      Files
        .lines(Path.of("docs", "readme.md"))
        .filter(line => line.startsWith("##"))
        .map(line => Content.fromLine(line).getOrElse(throw new Exception(s"Unsupported header: $line")).render)
        .skip(1) // drop the first entry i.e. 'Table of contents' itself
        .collect(Collectors.joining(System.lineSeparator()))

    println(tableOfContents)
  }

  inline def members[A] = ${ membersOf[A] }

  private def membersOf[A: Type](using Quotes) = {
    import quotes.reflect.*

    val members =
      TypeRepr
        .of[A]
        .typeSymbol
        .declaredMethods
        .map(meth => TypeRepr.of[A].memberType(meth))
        .map {
          case MethodType(params, tpes, ret) => s"params -> $params, argTypes -> ${tpes.map(_.show)}, ret -> ${ret.show}"
          case PolyType(names, bounds, MethodType(params, tpes, ret)) =>
            s"params -> $params, argTypes -> ${tpes.map(_.show)}, ret -> ${ret.show}"
          case other => other.show(using Printer.TypeReprStructure)
        }

    report.info(members.mkString("\n"))
    '{}
  }
}
