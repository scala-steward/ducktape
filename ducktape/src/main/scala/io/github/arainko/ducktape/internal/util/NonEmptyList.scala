package io.github.arainko.ducktape.internal.util

private[ducktape] opaque type NonEmptyList[+A] = ::[A]

private[ducktape] object NonEmptyList {
  import scala.collection.immutable.:: as Cons

  private def unsafeCoerce[A](list: List[A]) = list.asInstanceOf[NonEmptyList[A]]

  private[ducktape] def fromCons[A](cons: ::[A]): NonEmptyList[A] = cons

  private[ducktape] def apply[A](head: A, tail: A*): NonEmptyList[A] = Cons(head, List(tail*))

  private[ducktape] def fromList[A](list: List[A]): Option[NonEmptyList[A]] =
    PartialFunction.condOpt(list) { case cons @ (_ :: _) => fromCons(cons) }

  extension [A](self: NonEmptyList[A]) {
    export toList.{ reduceLeft, head, tail }

    private[ducktape] def toList: ::[A] = self

    private[ducktape] def ::(elem: A): NonEmptyList[A] = Cons(elem, self)

    private[ducktape] def :::(that: List[A]): NonEmptyList[A] = unsafeCoerce(toList ::: that)

    private[ducktape] def map[B](f: A => B): NonEmptyList[B] = unsafeCoerce(toList.map(f))

    private[ducktape] def reverse: NonEmptyList[A] = unsafeCoerce(toList.reverse)
  }
}
