package com.github.billkuang.collection

import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom

object Extensions {

  implicit class ScalaCollectionExtensions[A, I[_]](underlying: I[A])(implicit toTraversable: I[A] => Traversable[A]) {

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

  implicit class ScalaPairedCollectionExtensions[K, V, I[_]](underlying: I[(K, V)])(implicit toTraversable: I[(K, V)] => Traversable[(K, V)]) {

    def cogroup[W](other: I[(K, W)])(implicit
      f1: I[K] => Traversable[K],
      f2: I[V] => Traversable[V],
      f3: I[(K, W)] => Traversable[(K, W)],
      f4: I[(K, Seq[V])] => Traversable[(K, Seq[V])],
      f5: I[(K, Seq[W])] => Traversable[(K, Seq[W])],
      cbf1: CanBuildFrom[I[(K, V)], (K, (Seq[V], Seq[W])), I[(K, (Seq[V], Seq[W]))]],
      cbf2: CanBuildFrom[I[(K, V)], K, I[K]],
      cbf3: CanBuildFrom[I[(K, W)], K, I[K]],
      cbf4: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]],
      cbf5: CanBuildFrom[I[(K, W)], (K, Seq[W]), I[(K, Seq[W])]]): I[(K, (Seq[V], Seq[W]))] = {

      (this.getKeys(cbf2).to[Set] union other.getKeys(cbf3).to[Set]).map { key =>
        val value = (underlying.get(key).getOrElse(Seq.empty[V]), other.get(key).getOrElse(Seq.empty[W]))
        key -> value
      }(breakOut)
    }

    def countByKey(implicit
      impf: I[(K, Seq[V])] => Traversable[(K, Seq[V])],
      cbf: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]]): Map[K, Long] = {
      underlying
        .groupByKey
        .map { case (key, seq) => key -> seq.size.toLong }
        .toMap
    }

    def foldByKey(zeroValue: V)(f: (V, V) => V)(implicit
      impf: I[(K, Seq[V])] => Traversable[(K, Seq[V])],
      cbf1: CanBuildFrom[I[(K, V)], (K, V), I[(K, V)]],
      cbf2: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]]): I[(K, V)] = {
      underlying
        .groupByKey
        .map { case (key, values) => key -> values.foldLeft(zeroValue)(f) }(breakOut)
    }

    def getKeys(implicit cbf: CanBuildFrom[I[(K, V)], K, I[K]]): I[K] = underlying.map { _._1 }(breakOut)

    def getValues(implicit cbf: CanBuildFrom[I[(K, V)], V, I[V]]): I[V] = underlying.map { _._2 }(breakOut)

    def groupByKey(implicit cbf: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]]): I[(K, Seq[V])] = {
      underlying
        .groupBy { _._1 }
        .map { case (key, keyValuePairs) => key -> keyValuePairs.map(_._2).to[Vector] }(breakOut)
    }

    def join[W](other: I[(K, W)])(implicit
      f1: I[K] => Traversable[K],
      f2: I[V] => Traversable[V],
      f3: I[(K, W)] => Traversable[(K, W)],
      f4: I[(K, Seq[V])] => Traversable[(K, Seq[V])],
      f5: I[(K, Seq[W])] => Traversable[(K, Seq[W])],
      f6: I[(K, (Seq[V], Seq[W]))] => Traversable[(K, (Seq[V], Seq[W]))],
      cbf1: CanBuildFrom[I[(K, V)], (K, (Seq[V], Seq[W])), I[(K, (Seq[V], Seq[W]))]],
      cbf2: CanBuildFrom[I[(K, V)], K, I[K]],
      cbf3: CanBuildFrom[I[(K, W)], K, I[K]],
      cbf4: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]],
      cbf5: CanBuildFrom[I[(K, W)], (K, Seq[W]), I[(K, Seq[W])]],
      cbf6: CanBuildFrom[I[(K, V)], (K, (V, W)), I[(K, (V, W))]]): I[(K, (V, W))] = {

      (underlying cogroup other).flatMap { case (key, (underlyingValues, otherValues)) =>
        for {
          underlyingItem <- underlyingValues
          otherItem <- otherValues
        } yield (key, (underlyingItem, otherItem))
      }(breakOut)
    }

    def leftOuterJoin[W](other: I[(K, W)])(implicit
      f1: I[K] => Traversable[K],
      f2: I[V] => Traversable[V],
      f3: I[(K, W)] => Traversable[(K, W)],
      f4: I[(K, Seq[V])] => Traversable[(K, Seq[V])],
      f5: I[(K, Seq[W])] => Traversable[(K, Seq[W])],
      f6: I[(K, (Seq[V], Seq[W]))] => Traversable[(K, (Seq[V], Seq[W]))],
      cbf1: CanBuildFrom[I[(K, V)], (K, (Seq[V], Seq[W])), I[(K, (Seq[V], Seq[W]))]],
      cbf2: CanBuildFrom[I[(K, V)], K, I[K]],
      cbf3: CanBuildFrom[I[(K, W)], K, I[K]],
      cbf4: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]],
      cbf5: CanBuildFrom[I[(K, W)], (K, Seq[W]), I[(K, Seq[W])]],
      cbf6: CanBuildFrom[I[(K, V)], (K, (V, Option[W])), I[(K, (V, Option[W]))]]): I[(K, (V, Option[W]))] = {

      (underlying cogroup other).filter { case (key, (underlyingValues, otherValues)) =>
        underlyingValues.nonEmpty
      }.flatMap { case (key, (underlyingValues, otherValues)) =>
        if (otherValues.isEmpty) underlyingValues map { uv => key -> (uv, None) }
        else underlyingValues flatMap { uv => otherValues map { ov => key -> (uv, Option(ov)) } }
      }(breakOut)
    }

    def rightOuterJoin[W](other: I[(K, W)])(implicit
      f1: I[K] => Traversable[K],
      f2: I[V] => Traversable[V],
      f3: I[(K, W)] => Traversable[(K, W)],
      f4: I[(K, Seq[V])] => Traversable[(K, Seq[V])],
      f5: I[(K, Seq[W])] => Traversable[(K, Seq[W])],
      f6: I[(K, (Seq[V], Seq[W]))] => Traversable[(K, (Seq[V], Seq[W]))],
      cbf1: CanBuildFrom[I[(K, V)], (K, (Seq[V], Seq[W])), I[(K, (Seq[V], Seq[W]))]],
      cbf2: CanBuildFrom[I[(K, V)], K, I[K]],
      cbf3: CanBuildFrom[I[(K, W)], K, I[K]],
      cbf4: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]],
      cbf5: CanBuildFrom[I[(K, W)], (K, Seq[W]), I[(K, Seq[W])]],
      cbf6: CanBuildFrom[I[(K, V)], (K, (Option[V], W)), I[(K, (Option[V], W))]]): I[(K, (Option[V], W))] = {

      (underlying cogroup other).filter { case (key, (underlyingValues, otherValues)) =>
        otherValues.nonEmpty
      }.flatMap { case (key, (underlyingValues, otherValues)) =>
        if (underlyingValues.isEmpty) otherValues map { ov => key -> (None, ov)}
        else otherValues flatMap { ov => underlyingValues map { uv => key -> (Option(uv), ov) } }
      }(breakOut)
    }

    def reduceByKey(f: (V, V) => V)(implicit
      impf: I[(K, Seq[V])] => Traversable[(K, Seq[V])],
      cbf1: CanBuildFrom[I[(K, V)], (K, V), I[(K, V)]],
      cbf2: CanBuildFrom[I[(K, V)], (K, Seq[V]), I[(K, Seq[V])]]): I[(K, V)] = {
      underlying
        .groupByKey
        .map { case (key, values) => key -> values.reduce(f) }(breakOut)
    }

    private implicit def pairedTraversableToGroupedMap[A, B](pairedTraversable: I[(A, B)])(
      implicit f1: I[(A, Seq[B])] => Traversable[(A, Seq[B])],
               f2: I[(A, B)] => Traversable[(A, B)],
               cbf: CanBuildFrom[I[(A, B)], (A, Seq[B]), I[(A, Seq[B])]]
    ): Map[A, Seq[B]] = {
      pairedTraversable.groupByKey.toMap
    }
  }
}
