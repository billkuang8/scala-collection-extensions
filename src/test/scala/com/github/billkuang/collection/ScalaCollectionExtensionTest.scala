package com.github.billkuang.collection

import org.scalatest.Matchers
import org.junit.Test
import com.github.billkuang.collection.Extensions._

class ExtensionsTest extends Matchers {

  @Test
  def countByValueTest {
    val count = Seq(1, 1, 2, 2, 3, 3, 4, 4, 5, 5).countByValue()
    count.to[Vector].sorted should be (Seq((1, 2), (2, 2), (3, 2), (4, 2), (5, 2)))
  }

  @Test
  def keyByTest {
    val col = Seq(1, 2, 3).keyBy { _ * 2 }
    col.sorted should be (Seq((2, 1), (4, 2), (6, 3)))
  }

  @Test
  def sampleTest {
    val sampleWithReplacement = Seq(1, 2, 3, 4, 5, 6, 7).sample(withReplacement = true, sampleSize = 5)
    sampleWithReplacement foreach println
    sampleWithReplacement should have size (5)
    sampleWithReplacement.distinct.size should be <= 5

    val sampleWithoutReplacement = Seq(1, 2, 3, 4, 5, 6, 7).sample(withReplacement = false, sampleSize = 5)
    sampleWithoutReplacement foreach println
    sampleWithoutReplacement should have size (5)
    sampleWithoutReplacement.distinct.size should be (5)
  }
}
