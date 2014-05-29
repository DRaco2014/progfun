package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a smaller tree") {
    new TestTrees {
      assert(weight(t1) === 10)
    }
  }
  
  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t2) === 23)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  
  test("times on a trivial list"){    
    assert(times(List('a')) == List(('a',1)),"invalid times!")
  }
  
  test("times on simple list"){    
    assert(times(List('a','b','a')) == List(('a',2),('b',1)),"invalid times!")
  }
  
  test("times on a crazy list"){    
    assert(times(List('a','b','a','c','d','b','a')) == List(('a',3),('b',2),('c',1),('d',1)),"invalid times!")
  }

  test("makeOrderedLeafList for empty table") {
    assert(makeOrderedLeafList(List()) === List(),"list should be empty")
  }
  
  test("makeOrderedLeafList for one element table") {
    assert(makeOrderedLeafList(List(('a',4))) === List(Leaf('a',4)),"list should contains one leaf")
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
