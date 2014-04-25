package com.github.billkuang.collection.extensions

import com.github.billkuang.collection.extensions.ScalaNumericCollectionExtensions.BinRange

class ScalaNumericCollectionExtensions[T, I[_]](underlying: I[T])(implicit numeric: Numeric[T], f: I[T] => Traversable[T]) {

  def mean: Double = {
    if (underlying.size == 0) return 0.0
    val total = underlying.map { numeric.toDouble(_) }.sum
    total / underlying.size.toDouble
  }

  def sampleVariance: Double = {
    if (underlying.size == 0) return 0.0
    val avg = underlying.mean
    val sizeMinusOne = underlying.size - 1
    underlying.map { v => math.pow((numeric.toDouble(v) - avg), 2) / sizeMinusOne }.sum
  }

  def populationVariance: Double = {
    if (underlying.size == 0) return 0.0
    val avg = underlying.mean
    val size = underlying.size
    underlying.map { v => math.pow((numeric.toDouble(v) - avg), 2) / size }.sum
  }

  def sampleStd: Double = math.sqrt(sampleVariance)

  def populationStd: Double = math.sqrt(populationVariance)

  def histogram(numOfBuckets: Int): Map[BinRange, Long] = {
    if (underlying.size == 0) return Map.empty[BinRange, Long]
    val min = numeric.toDouble(underlying.min)
    val max = numeric.toDouble(underlying.max)
    val increment = (max - min) / numOfBuckets.toDouble
    val binRanges = new scala.collection.mutable.ArrayBuffer[BinRange]()
    var curr = min
    while (curr < max) {
      binRanges += BinRange(curr, curr + increment)
      curr += increment
    }
    var underlyingVector = underlying.to[Vector].sorted
    binRanges.map { bin =>
      var count = 0L
      while (underlyingVector.nonEmpty && bin.includes(numeric.toDouble(underlyingVector.head))) {
        count += 1L
        underlyingVector = underlyingVector.drop(1)
      }
      bin -> count
    }.toMap
  }
}

object ScalaNumericCollectionExtensions {
  case class BinRange(lowerBound: Double, upperBound: Double) {
    def includes(num: Double): Boolean = lowerBound <= num && num <= upperBound
  }
}
