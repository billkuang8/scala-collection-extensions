package com.github.billkuang.collection.extensions

import scala.collection.generic.CanBuildFrom
import scala.collection._

class ScalaCollectionExtensions[A, I[_]](underlying: I[A])(implicit toTraversable: I[A] => Traversable[A]) {

  def countByValue(): Map[A, Long] = {
    underlying
      .groupBy { identity }
      .map { case (value, seq) => value -> seq.size.toLong }
      .toMap
  }

  def keyBy[B](f: A => B)(implicit cbf: CanBuildFrom[I[A], (B, A), I[(B, A)]]): I[(B, A)] = {
    underlying.map { item => f(item) -> item }(breakOut)
  }

  def sample(withReplacement: Boolean, sampleSize: Int)(implicit cbf: CanBuildFrom[I[A], A, I[A]]): I[A] = {

    def sampleWithReplacement: I[A] = {
      val underlyingVector = underlying.to[Vector]
      underlying
        .map { i => i -> underlyingVector(new java.util.Random().nextInt(underlyingVector.size)) }
        .take(sampleSize)
        .map { _._2 }(breakOut)
    }

    def sampleWithoutReplacement: I[A] = {
      underlying.to[Vector]
        .map { i => i -> new java.util.Random().nextDouble() }
        .sortBy { _._2 }
        .take(sampleSize)
        .map { _._1 }(breakOut)
    }

    if (withReplacement) sampleWithReplacement
    else sampleWithoutReplacement
  }
}
