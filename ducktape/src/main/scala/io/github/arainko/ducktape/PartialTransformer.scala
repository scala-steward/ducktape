package io.github.arainko.ducktape

import scala.deriving.Mirror

/*
  DSL:
    final case class BetterInt private (value: Int)

    object BetterInt {
      def create(int: Int): Option[BetterInt] = ???
    }

    val transformer: PartialTransformer[Option, Int, BetterInt] = BetterInt.create

 */

object PartialTransformer {
  trait FailFast[F[+x], Source, Dest] {
    def transform(value: Source): F[Dest]
  }

  object FailFast {
    given partialFromTotal[F[+x], Source, Dest](using
      total: Transformer[Source, Dest],
      support: Support[F]
    ): FailFast[F, Source, Dest] =
      new {
        def transform(value: Source): F[Dest] = support.pure(total.transform(value))
      }

    given derived[F[+x], Source, Dest](using
      Source: Mirror.ProductOf[Source],
      Dest: Mirror.ProductOf[Dest],
      support: Support[F]
    ): FailFast[F, Source, Dest] = ???

    trait Support[F[+x]] {
      def pure[A](value: A): F[A]
      def map[A, B](fa: F[A], f: A => B): F[B]
      def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
    }

    object Support {
      given optionFailFastSupport: Support[Option] =
        new {
          def pure[A](value: A): Option[A] = Some(value)
          def map[A, B](fa: Option[A], f: A => B): Option[B] = fa.map(f)
          def flatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = fa.flatMap(f)
        }

      given eitherFailFastSupport[E]: Support[[A] =>> Either[E, A]] =
        new {
          def pure[A](value: A): Either[E, A] = Right(value)
          def map[A, B](fa: Either[E, A], f: A => B): Either[E, B] = fa.map(f)
          def flatMap[A, B](fa: Either[E, A], f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
        }
    }
  }

  trait Accumulating[F[+x], Source, Dest] {
    def transform(value: Source): F[Dest]
  }

  object Accumulating {

    given partialFromTotal[F[+x], Source, Dest](using
      total: Transformer[Source, Dest],
      support: Support[F]
    ): Accumulating[F, Source, Dest] =
      new {
        def transform(value: Source): F[Dest] = support.pure(total.transform(value))
      }

    trait Support[F[+x]] {
      def pure[A](value: A): F[A]
      def map[A, B](fa: F[A], f: A => B): F[B]
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    given eitherConsAccumulatingSupport[E]: Support[[A] =>> Either[::[E], A]] =
      new {
        def pure[A](value: A): Either[::[E], A] = Right(value)
        def map[A, B](fa: Either[::[E], A], f: A => B): Either[::[E], B] = fa.map(f)
        def product[A, B](fa: Either[::[E], A], fb: Either[::[E], B]): Either[::[E], (A, B)] =
          (fa, fb) match {
            case (Right(a), Right(b))           => Right(a -> b)
            case (Right(_), err @ Left(_))      => err.asInstanceOf[Either[::[E], (A, B)]]
            case (err @ Left(_), Right(_))      => err.asInstanceOf[Either[::[E], (A, B)]]
            case (Left(errorsA), Left(errorsB)) => Left((errorsA ::: errorsB).asInstanceOf[::[E]])
          }
      }
  }
}
