package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends GameDef with Solver with InfiniteTerrain {
    override val startPos = Pos(0, 0)
    override val goal = Pos(0, 4)
  }

  trait ImpossibruLevel extends SolutionChecker {
    val level =
    """ooo-------
      |oSo-------
      |ooo-------
      |------ooo-
      |------oTo-
      |------ooo-""".stripMargin
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,-1)), "-1,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("block isStanding") {
    new Level1 {
      assert(Block(Pos(1, 2), Pos(1, 2)).isStanding, "(1, 2), (1, 2)") 
      assert(!Block(Pos(1, 2), Pos(2, 2)).isStanding, "(1, 2), (2, 2)") 
      assert(!Block(Pos(1, 2), Pos(3, 2)).isStanding, "(1, 2), (3, 2)") 
    } 
  }

  test("block isLegal") {
    new Level1 {
      assert(Block(Pos(1, 2), Pos(1, 2)).isLegal, "(1, 2), (1, 2)") 
      assert(Block(Pos(1, 2), Pos(2, 2)).isLegal, "(1, 2), (2, 2)") 
      assert(!Block(Pos(2, 0), Pos(3, 0)).isLegal, "(2, 0), (3, 0)")
      assert(!Block(Pos(0, 3), Pos(0, 4)).isLegal, "(0, 3), (0, 4)")
    } 
  }

  test("startBlock") {
    new Level1 {
      assert(startBlock === Block(Pos(1, 1), Pos(1, 1)))
    } 
  }

  test("neighbors") {
    new Level1 {
      val allNeighbours = List(
        Block(Pos(3, 0), Pos(3, 1)) -> Left,
        Block(Pos(3, 3), Pos(3, 4)) -> Right,
        Block(Pos(1, 2), Pos(2, 2)) -> Up,
        Block(Pos(4, 2), Pos(5, 2)) -> Down
      )
      assert(Block(Pos(3, 2), Pos(3, 2)).neighbors === allNeighbours)
    }
  }

  test("legalNeighbors") {
    new Level1 {
      val legalNeighbors = List(
        Block(Pos(3, 3), Pos(3, 4)) -> Right,
        Block(Pos(1, 2), Pos(2, 2)) -> Up
      )
      assert(Block(Pos(3, 2), Pos(3, 2)).legalNeighbors === legalNeighbors)
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val expected = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
      val result = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet
      assert(result === expected)
    } 
  }

  test("newNeighborsOnly") {
    new Level1 {
      val result = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      ).toSet
      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(result === expected)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }

  test("solve on infinite terrain") {
    new Level2 {
      assert(solution.length > 0)
    }
  }

  test("impossible level") {
    new ImpossibruLevel {
      assert(solution.isEmpty)
    }
  }
}
