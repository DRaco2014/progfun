package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  trait TestSets {
    //input
    val abc = List(('a', 1), ('b', 1), ('c', 1))
    
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val larda = List(('a', 2), ('d', 1), ('l', 1), ('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    
    val ab: List[List[(Char, Int)]] = List(List(('a', 1)), List(('b', 1)), List(('a', 1), ('b', 1)),List())
    val c: List[List[(Char, Int)]] = List(List(('c', 1)),List())
    
    //output
    val abccomb = List(
      List(),
      List(('a', 1)), List(('b', 1)), List(('c', 1)),
      List(('a', 1), ('b', 1)), List(('a', 1), ('c', 1)), List(('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 1)))

    
  }
  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: married") {
    assert(wordOccurrences("married") === List(('a', 1), ('d', 1), ('e', 1), ('i', 1), ('m', 1), ('r', 2)))
  }

  test("wordOccurrences: empty String") {
    assert(wordOccurrences("") === List())
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("dictionaryByOccurrences.get: married") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('d', 1), ('e', 1), ('i', 1), ('m', 1), ('r', 2))).map(_.toSet) === Some(Set("married", "admirer")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)))
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: abc") {
    new TestSets {
      assert(combinations(abc).toSet === abccomb.toSet)
    }
  }
  test("reflexive merge and combine: ab ++ c == c ++ ab") {
    new TestSets {
      assert(mergeAndCombine(ab, c).toSet === mergeAndCombine(c, ab).toSet)
    }
  }  
  test("merge and combine: ab ++ c") {
    new TestSets {
      assert(mergeAndCombine(ab, c).toSet === abccomb.toSet)
    }
  }
  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }
  
  test("subtract: lard - rd") {
    new TestSets {
    val rd = List(('r', 1),('d', 1))
    assert(subtract(lard, rd)  === List(('a', 1),('l', 1)))
    }
  }
  
  test("subtract: larda - ra") {
    new TestSets {
    val ar = List(('a', 1),('r', 1))
    
    assert(subtract(larda, ar) === lad)
    }
  }
  
  test("subtract: a - a") {
    val a1 = List(('a', 1))
    val a2 = List(('a', 1))
    
    assert(subtract(a1, a2) === List())
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }
  
  test("sentence anagrams: abba") {
    val sentence = List("abba")
    val res = List(
    List("Abba")    
    )
    assert(sentenceAnagrams(sentence).toSet === res.toSet)
}
}
