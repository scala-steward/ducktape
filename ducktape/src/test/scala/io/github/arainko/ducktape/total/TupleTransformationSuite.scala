package io.github.arainko.ducktape.total

import io.github.arainko.ducktape.*
import munit.Location
import munit.Compare

class TupleTransformationSuite extends DucktapeSuite {

  test("tuple-to-tuple works") {
    val source = (1, 1, List(1), (1, 2, 3))
    val expected = (1, Some(1), Vector(1), (1, 2))
    assertTransforms(source, expected)
  }

  test("tuple-to-product works") {
    case class DestToplevel(int: Int, opt: Option[Int], coll: Vector[Int], level1: DestLevel1)
    case class DestLevel1(int1: Int, int2: Int)

    val source = (1, 1, List(1), (1, 2, 3))
    val expected = DestToplevel(1, Some(1), Vector(1), DestLevel1(1, 2))
    assertTransforms(source, expected)
  }

  test("product-to-tuple works") {
    case class DestToplevel(int: Int, opt: Int, coll: Vector[Int], level1: DestLevel1)
    case class DestLevel1(int1: Int, int2: Int, int3: Int)

    val source = DestToplevel(1, 1, Vector(1), DestLevel1(1, 2, 3))
    val expected = (1, Option(1), List(1), (1, 2))
    assertTransforms(source, expected)
  }

  test("tuple-to-function works") {
    def method(arg1: Int, arg2: Option[Int], coll: Vector[Int], level1: (Int, Int)) = (arg1, arg2, coll, level1)
    val source: (Int, Int, List[Int], (Int, Int, Int)) = (1, 1, List(1), (1, 2, 3))
    val expected = (1, Some(1), Vector(1), (1, 2))

    assertEachEquals(
      source.via(method),
      Transformer.defineVia[(Int, Int, List[Int], (Int, Int, Int))](method).build().transform(source)
    )(expected)
  }

  test("tuple-to-tuple can be configured") {
    val source = (1, 1, List(1), (1, 2, 3))
    val expected: (Int, Option[(Int, Int)], Vector[Int], (Int, Int, Int, Int)) = (1, Option((1, 2)), Vector(1), (1, 2, 3, 4))

    assertTransformsConfigured(source, expected)(
      Field.const(_.apply(3).apply(3), 4),
      Field.computed(_.apply(1).element, elem => (elem._2, 2))
    )
  }

  test("plain tuples can be configured with _-accessors") {
    val source = (1, 1, List(1), (1, 2, 3))
    val expected: (Int, Option[(Int, Int)], Vector[Int], (Int, Int, Int, Int)) = (1, Option((1, 2)), Vector(1), (1, 2, 3, 4))

    assertTransformsConfigured(source, expected)(
      Field.const(_._4._4, 4),
      Field.computed(_._2.element, elem => (elem._2, 2))
    )
  }
}
