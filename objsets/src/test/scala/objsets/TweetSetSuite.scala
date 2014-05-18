package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6c = set1.incl(c)
    val set6d = set6c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: should found nothing on non-empty set") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "elmo")) === 0)
    }
  }

  test("filter: should found something on non-root tweet") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "b")) === 1)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: set4c and itself") {
    new TestSets {
      assert(size(set4c.union(set4c)) === 3)
    }
  }

  test("union: empty set and itself") {
    new TestSets {
      assert(size(set1.union(set1)) === 0)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: with single element set") {
    new TestSets {
      assert(set2.mostRetweeted.user === "a")
    }
  }

  test("mostRetweeted: with many elements set") {
    new TestSets {
      assert(set6d.mostRetweeted.user === "d")
    }
  }

  test("mostRetweeted: with empty set") {
    new TestSets {
      intercept[java.util.NoSuchElementException] {
        set1.mostRetweeted
        fail("Should have thrown an exception")
      }
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
