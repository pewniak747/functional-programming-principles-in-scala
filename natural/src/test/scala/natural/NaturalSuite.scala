package natural

import org.scalatest.FunSuite

class ZeroSuite extends FunSuite {
  val zero = new Zero
  val one = new Successor(zero)
  val two = new Successor(one)

  test("isZero") {
    assert(zero.isZero, true)
  }

  test("predecessor") {
    intercept[NoSuchNumber] {
      zero.predecessor
    }
  }

  test("successor") {
    assert(zero.successor == one)
  }

  test("+") {
    assert(zero + one == one)
    assert(zero + two == two)
  }

  test("-") {
    assert(zero - zero == zero)
    intercept[NoSuchNumber] {
      zero - one
    }
  }
}

class SuccessorSuite extends FunSuite {
  val zero = new Zero
  val one = new Successor(zero)
  val two = new Successor(one)
  val three = new Successor(two)

  test("isZero") {
    assert(!one.isZero)  
    assert(!two.isZero)  
  }

  test("predecessor") {
    assert(one.predecessor == zero)
    assert(two.predecessor == one)
  }

  test("successor") {
    assert(one.successor == two)
    assert(two.successor == three)
  }

  test("+") {
    assert(one + one == two, "1 + 1 == 2")
    assert(one + two == three, "1 + 2 == 3")
    assert(two + one == three, "2 + 1 == 3")
    assert(one + zero == one, "1 + 0 == 1")
  }

  test("-") {
    assert(three - zero == three, "3 - 0 == 3")
    assert(three - two == one, "3 - 2 == 1")
    assert(three - one == two, "3 - 1 == 2")
    assert(three - three == zero, "3 - 1 == 0")
    intercept[NoSuchNumber] {
      one - three
    }
  }
}
