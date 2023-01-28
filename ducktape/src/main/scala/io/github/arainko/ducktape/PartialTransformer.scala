package io.github.arainko.ducktape

import scala.deriving.Mirror
import scala.collection.Factory

object PartialTransformer {
  trait FailFast[F[+x], Source, Dest] {
    def transform(value: Source): F[Dest]
  }

  // TODO: Move these out to separate files (both FailFast and Accumulating)
  object FailFast {
    given partialFromTotal[F[+x], Source, Dest](using
      total: Transformer[Source, Dest],
      F: Support[F]
    ): FailFast[F, Source, Dest] =
      new {
        def transform(value: Source): F[Dest] = F.pure(total.transform(value))
      }

    given betweenOption[F[+x], Source, Dest](using
      transformer: PartialTransformer.FailFast[F, Source, Dest],
      F: Support[F]
    ): FailFast[F, Option[Source], Option[Dest]] =
      new {
        def transform(value: Option[Source]): F[Option[Dest]] =
          value.fold(F.pure(None))(source => F.map(transformer.transform(source), Some.apply))
      }

    given betweenNonOptionOption[F[+x], Source, Dest](using
      transformer: PartialTransformer.FailFast[F, Source, Dest],
      F: Support[F]
    ): FailFast[F, Source, Option[Dest]] =
      new {
        def transform(value: Source): F[Option[Dest]] = F.map(transformer.transform(value), Some.apply)
      }

    // very-very naive impl, can probably lead to stack overflows given big enough collections and a data type that is not stack safe...
    given betweenCollections[F[+x], Source, Dest, SourceColl[x] <: Iterable[x], DestColl[x] <: Iterable[x]](using
      transformer: PartialTransformer.FailFast[F, Source, Dest],
      F: Support[F],
      factory: Factory[Dest, DestColl[Dest]]
    ): FailFast[F, SourceColl[Source], DestColl[Dest]] =
      new {
        def transform(value: SourceColl[Source]): F[DestColl[Dest]] = {
          val traversed = value.foldLeft(F.pure(factory.newBuilder)) { (builder, elem) =>
            F.flatMap(builder, currentBuilder => F.map(transformer.transform(elem), currentBuilder += _))
          }
          F.map(traversed, _.result())
        }
      }

    given derived[F[+x], Source, Dest](using
      Source: Mirror.ProductOf[Source],
      Dest: Mirror.ProductOf[Dest],
      F: Support[F]
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
      F: Support[F]
    ): Accumulating[F, Source, Dest] =
      new {
        def transform(value: Source): F[Dest] = F.pure(total.transform(value))
      }

    given betweenOption[F[+x], Source, Dest](using
      transformer: PartialTransformer.Accumulating[F, Source, Dest],
      F: Support[F]
    ): Accumulating[F, Option[Source], Option[Dest]] =
      new {
        def transform(value: Option[Source]): F[Option[Dest]] =
          value.fold(F.pure(None))(source => F.map(transformer.transform(source), Some.apply))
      }

    given betweenNonOptionOption[F[+x], Source, Dest](using
      transformer: PartialTransformer.Accumulating[F, Source, Dest],
      F: Support[F]
    ): Accumulating[F, Source, Option[Dest]] =
      new {
        def transform(value: Source): F[Option[Dest]] = F.map(transformer.transform(value), Some.apply)
      }

    // very-very naive impl, can probably lead to stack overflows given big enough collections and a data type that is not stack safe...
    given betweenCollections[F[+x], Source, Dest, SourceColl[x] <: Iterable[x], DestColl[x] <: Iterable[x]](using
      transformer: PartialTransformer.Accumulating[F, Source, Dest],
      F: Support[F],
      factory: Factory[Dest, DestColl[Dest]]
    ): Accumulating[F, SourceColl[Source], DestColl[Dest]] =
      new {
        def transform(value: SourceColl[Source]): F[DestColl[Dest]] = {
          val traversed = value.foldLeft(F.pure(factory.newBuilder)) { (builder, elem) =>
            F.map(F.product(builder, transformer.transform(elem)), _ += _)
          }
          F.map(traversed, _.result())
        }
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

    given eitherIterableAccumulatingSupport[E, Coll[x] <: Iterable[x]](using factory: Factory[E, Coll[E]]): Support[[A] =>> Either[Coll[E], A]] =
      new {
        override def pure[A](value: A): Either[Coll[E], A] = Right(value)
        override def map[A, B](fa: Either[Coll[E], A], f: A => B): Either[Coll[E], B] = fa.map(f)
        override def product[A, B](fa: Either[Coll[E], A], fb: Either[Coll[E], B]): Either[Coll[E], (A, B)] = 
          (fa, fb) match {
            case (Right(a), Right(b))           => Right(a -> b)
            case (Right(_), err @ Left(_))      => err.asInstanceOf[Either[Coll[E], (A, B)]]
            case (err @ Left(_), Right(_))      => err.asInstanceOf[Either[Coll[E], (A, B)]]
            case (Left(errorsA), Left(errorsB)) => 
              val builder = factory.newBuilder
              val accumulated = builder ++= errorsA ++= errorsB
              Left(accumulated.result())
          }
      }
  }
}
