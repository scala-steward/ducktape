package io.github.arainko.ducktape

import munit.{ FunSuite, Location }
import munit.Compare

trait DucktapeSuite extends FunSuite {
  def assertEachEquals[Source, Dest](head: Source, tail: Source*)(expected: Dest)(using Location, Compare[Source, Dest]) = {
    (head :: tail.toList).foreach(actual => assertEquals(actual, expected))
  }

  transparent inline def assertFailsToCompile(inline code: String)(using Location) = {
    assert(compiletime.testing.typeChecks(code), "Code snippet compiled despite expecting not to")
  }

  transparent inline def assertFailsToCompileWith(inline code: String)(expected: String*)(using Location) = {
    val errors = compiletime.testing.typeCheckErrors(code).map(_.message).toSet
    assertEquals(errors, expected.toSet, "Error did not contain expected value")
  }
}
