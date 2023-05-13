package io.github.arainko.ducktape.internal.util

extension [A, B](func: A ?=> B) private[ducktape] def map[C](f: B => C): A ?=> C =
  a ?=> f(func(using a))
