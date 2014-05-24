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

    val one_el = Nil.append(new Tweet("e", "e body", 14))
    val two_el = one_el.append(new Tweet("f", "f body", 17))
    val three_el = two_el.append(new Tweet("g", "g body", 21))
    
    val tweetSets = set1.incl(new Tweet("John", "Scala is fun", 14)).incl(new Tweet("mark", "I agree with @Johh, Scala is awesome", 3)).incl(new Tweet("Amy", "What do you think about Scala", 21))
    val keywords = List("Scala")

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

  test("append: to an empty set") {
    new TestSets {
      assert(!one_el.isEmpty, "it should not be empty")
      assert(one_el.head.user == "e", "head is incorrect")
      assert(one_el.tail.isEmpty, "tail should be empty")
    }
  }

  test("append: to an one element set") {
    new TestSets {
      assert(!two_el.isEmpty, "it should not be empty")
      assert(two_el.head.user == "e", "head is incorrect")
      assert(!two_el.tail.isEmpty, "tail should not be empty")
      assert(two_el.tail.head.user == "f", "tail's head is incorrect")      
    }
  }
  
    test("append: to a two element set") {
    new TestSets {
      assert(!three_el.isEmpty, "it should not be empty")
      assert(three_el.head.user == "e", "head is incorrect")
      assert(!three_el.tail.isEmpty, "tail should not be empty")
      assert(three_el.tail.head.user == "f", "tail's head is incorrect")
      assert(!three_el.tail.tail.isEmpty, "tail's tail should not be empty")
      assert(three_el.tail.tail.head.user == "g", "tail's tail's head is incorrect")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty, "sorted list should not be empty")
      assert(trends.head.user == "a" || trends.head.user == "b", "1st is wrong")
      assert(trends.tail.head.user == "a" || trends.tail.head.user == "b", "2nd is wrong")
      assert(trends.tail.tail.head.user == "d", "3rd is wrong")
      assert(trends.tail.tail.tail.head.user == "c", "4th is wrong")

    }
  }
  
    test("filtering: on 3-element TweetSet and one keyword") {
    new TestSets {
      val scalaTweets = tweetSets.filter(t => keywords.exists(s => t.text.contains(s)))
      val trending = scalaTweets.descendingByRetweet
      assert(!trending.isEmpty,"it should not be empty")            
      assert(trending.head.user == "Amy","Head element is incorect")
      assert(!trending.tail.isEmpty,"tail should not be empty")
      assert(trending.tail.head.user == "John","Tail's Head element is incorect")
    }
  }
    
}
