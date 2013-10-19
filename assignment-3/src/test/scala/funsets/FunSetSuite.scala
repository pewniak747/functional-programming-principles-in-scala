package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  trait LargeTestSets {
    val positive : Set = (x: Int) => x > 0
    val negative : Set = (x: Int) => x < 0
    val even : Set = (x: Int) => x % 2 == 0
  }

  test("intersection contains common elements") {
    new LargeTestSets {
      val positiveEvens = intersect(positive, even)
      assert(!contains(intersect(positive, negative), 0), "empty set")
      assert(contains(positiveEvens, 2), "all positive even")
      assert(!contains(positiveEvens, 1), "all positive even does not contain odd")
    }
  }

  test("diff contains elements from first set but not second set") {
    new LargeTestSets {
      val positiveNotEven = diff(positive, even)
      assert(!contains(positiveNotEven, -1), "does not contain negative number")
      assert(!contains(positiveNotEven, 0), "does not contain 0")
      assert(contains(positiveNotEven, 1), "contains positive odd number")
      assert(!contains(positiveNotEven, 2), "does not contain positive even number")
    } 
  }

  test("filters return a subset of a set for which a predicate holds") {
    new LargeTestSets {
      val positiveThreeDivisors = filter(positive, _ % 3 == 0)
      assert(!contains(positiveThreeDivisors, 1))
      assert(!contains(positiveThreeDivisors, 2))
      assert(contains(positiveThreeDivisors, 3))
    } 
  }

  test("forall returns whether all bounded integers in a set satisfy predicate") {
    new LargeTestSets {
      assert(forall(positive, _ > 0))
      assert(forall(negative, _ < 0))
      assert(!forall(positive, _ < 999))
      assert(!forall(positive, _ % 2 == 0))
    }
  }

  test("exists returns whether there exists a bounded integer that satisfies a predicate") {
    new LargeTestSets {
      assert(exists(positive, _ % 999 == 0))
      assert(exists(even, _ > 50))
      assert(!exists(negative, _ >= 0))
      assert(!exists(even, _ % 2 == 1))
    }
  }

  test("map returns a set transformed by applying a function") {
    new LargeTestSets {
      val squaresOfNumbers = map(positive, (x: Int) => x * x)
      assert(contains(squaresOfNumbers, 1))
      assert(contains(squaresOfNumbers, 4))
      assert(contains(squaresOfNumbers, 9))
      assert(!contains(squaresOfNumbers, 10))
    }
  }
}
