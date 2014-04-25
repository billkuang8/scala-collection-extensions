package com.github.billkuang.collection

package object extensions {

  implicit def toScalaCollectionExtensions[A, I[_]](underlying: I[A])(implicit toTraversable: I[A] => Traversable[A]) = {
    new ScalaCollectionExtensions(underlying)
  }

  implicit def toScalaPairedCollectionExtensions[K, V, I[_]](underlying: I[(K, V)])(implicit toTraversable: I[(K, V)] => Traversable[(K, V)]) = {
    new ScalaPairedCollectionExtensions(underlying)
  }

  implicit def toScalaNumericCollectionExtensions[T, I[_]](underlying: I[T])(implicit numeric: Numeric[T], f: I[T] => Traversable[T]) = {
    new ScalaNumericCollectionExtensions(underlying)
  }
}
