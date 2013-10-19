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

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
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

  test("times converts a list of characters into a frequency list") {
    assert(times(List('h', 'e', 'l', 'l', 'o')) === List(('h', 1), ('e', 1), ('l', 2), ('o', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    assert(singleton(List[Leaf]()) === false)
    assert(singleton(List(Leaf('a', 1))) === true)
    assert(singleton(List(Leaf('a', 1), Leaf('b', 2))) === false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("badabad".toList)) === "badabad".toList)
    }
  }

  test("decode french secret") {
    assert(decodedSecret.mkString === "huffmanestcool")
  }

  test("codeBits") {
    val myTable: CodeTable = List[(Char, List[Bit])](('a', List(0)), ('b', List(1)))
    assert(codeBits(myTable)('a') === List(0))
    assert(codeBits(myTable)('b') === List(1))
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) === List[(Char, List[Bit])](('a', List(0)), ('b', List(1))))
    }
  }

  test("codeBits & convert") {
    new TestTrees {
      assert(codeBits(convert(t1))('a') === List[Bit](0))
      assert(codeBits(convert(t1))('b') === List[Bit](1))
      assert(codeBits(convert(t2))('a') === List[Bit](0, 0))
      assert(codeBits(convert(t2))('b') === List[Bit](0, 1))
      assert(codeBits(convert(t2))('d') === List[Bit](1))
    }
  }

  test("encode using quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, quickEncode(t2)("badabad".toList)) === "badabad".toList)
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }
}
