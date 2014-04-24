package com.github.billkuang.collection

import org.scalatest.Matchers
import org.junit.Test
import com.github.billkuang.collection.Extensions._

class ScalaPairedCollectionExtensionTest extends Matchers {

  @Test
  def cogroupTest {
    val col1 = Seq(1 -> 2, 2 -> 3, 3 -> 4)
    val col2 = Seq(2 -> "3", 3 -> "4", 4 -> "5")
    val res = col1.cogroup(col2).toMap
    res(1) should be ((Seq(2), Seq.empty))
    res(2) should be ((Seq(3), Seq("3")))
    res(3) should be ((Seq(4), Seq("4")))
    res(4) should be ((Seq.empty, Seq("5")))
  }

  @Test
  def countByKeyTest {
    val col = Seq(1 -> 2, 2 -> 3, 3 -> 4, 1 -> 2, 2 -> 3, 3 -> 4)
    val res = col.countByKey.to[Vector]
    res.sorted should be (Seq(1 -> 2, 2 -> 2, 3 -> 2))
  }

  @Test
  def foldByKeyTest {
    val col = Seq(1 -> 2, 2 -> 3, 3 -> 4, 1 -> 2, 2 -> 3, 3 -> 4)
    val res = col.foldByKey(0) { _ + _ }
    res.sorted should be (Seq(1 -> 4, 2 -> 6, 3 -> 8))
  }

  @Test
  def getKeysTest {
    val col = Seq(1 -> 2, 2 -> 3, 3 -> 4)
    val res = col.getKeys
    res should be (Seq(1, 2, 3))
  }

  @Test
  def getValuesTest {
    val col = Seq(1 -> 2, 2 -> 3, 3 -> 4)
    val res = col.getValues
    res should be (Seq(2, 3, 4))
  }

  @Test
  def groupByKeyTest {
    val col = Seq(1 -> 2, 2 -> 3, 3 -> 4, 3 -> 5)
    val res  = col.groupByKey
    res should contain (1 -> Seq(2))
    res should contain (2 -> Seq(3))
    res should contain (3 -> Seq(4, 5))
  }

  @Test
  def joinTest {
    val col1 = Seq(1 -> 2, 2 -> 3, 3 -> 4)
    val col2 = Seq(2 -> "3", 3 -> "4", 4 -> "5")
    val res = col1.join(col2).toMap
    res should have size (2)
    res(2) should be ((3, "3"))
    res(3) should be ((4, "4"))
  }

  @Test
  def leftOuterJoinTest {
    val col1 = Seq(1 -> 2, 2 -> 3, 3 -> 4)
    val col2 = Seq(2 -> "3", 3 -> "4", 4 -> "5")
    val res = col1.leftOuterJoin(col2).toMap
    res should have size (3)
    res(1) should be ((2, None))
    res(2) should be ((3, Some("3")))
    res(3) should be ((4, Some("4")))
  }

  @Test
  def rightOuterJoinTest {
    val col1 = Seq(1 -> 2, 2 -> 3, 3 -> 4)
    val col2 = Seq(2 -> "3", 3 -> "4", 4 -> "5")
    val res = col1.rightOuterJoin(col2).toMap
    res should have size (3)
    res(2) should be ((Some(3), "3"))
    res(3) should be ((Some(4), "4"))
    res(4) should be ((None, "5"))
  }

  @Test
  def reduceByKeyTest {
    val col = Seq(1 -> 2, 2 -> 3, 3 -> 4, 1 -> 2, 2 -> 3, 3 -> 4)
    val res = col.reduceByKey { _ + _ }
    res.sorted should be (Seq(1 -> 4, 2 -> 6, 3 -> 8))
  }
}
