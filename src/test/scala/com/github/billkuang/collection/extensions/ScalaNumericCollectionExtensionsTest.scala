package com.github.billkuang.collection.extensions

import org.scalatest.Matchers
import org.junit.Test
import com.github.billkuang.collection.extensions.ScalaNumericCollectionExtensions.BinRange

class ScalaNumericCollectionExtensionsTest extends Matchers {

  val collection = Seq(1, 2, 3, 4, 5)

  @Test
  def meanTest {
    collection.mean should be (3.0)
  }

  @Test
  def varianceTest {
    collection.sampleVariance should be ((4.0 + 1.0 + 0.0 + 1.0 + 4.0) / 4.0)
    collection.populationVariance should be ((4.0 + 1.0 + 0.0 + 1.0 + 4.0) / 5.0)
  }

  @Test
  def standardDeviationTest {
    collection.sampleStd should be (math.sqrt((4.0 + 1.0 + 0.0 + 1.0 + 4.0) / 4.0))
    collection.populationStd should be (math.sqrt((4.0 + 1.0 + 0.0 + 1.0 + 4.0) / 5.0))
  }

  @Test
  def histogramTest {
    val histogram = collection.histogram(numOfBuckets = 4)
    histogram(BinRange(1.0, 2.0)) should be (2L)
    histogram(BinRange(2.0, 3.0)) should be (1L)
    histogram(BinRange(3.0, 4.0)) should be (1L)
    histogram(BinRange(4.0, 5.0)) should be (1L)
  }
}
