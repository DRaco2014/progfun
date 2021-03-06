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
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait LevelA extends SolutionChecker {
    val level =
      """ST
      |oo
      |oo""".stripMargin

    val initialStream  = Stream((startBlock, List()))
  }
  
  trait LevelB extends SolutionChecker {
    val level =
      """SoT
      |ooo
      |oo-""".stripMargin

    val initial  = Stream((startBlock, List()))
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
    val terra = Vector(level.split("\n").map(str => Vector(str: _*)): _*)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(3, 0)), "3,0")
      assert(!terrain(Pos(-1, 0)), "should detect invalid pos (-1,0)")
      assert(!terrain(Pos(-1, -1)), "should detect invalid pos (-1,-1)")
      assert(!terrain(Pos(1, -2)), "should detect invalid pos (1,-2)")
      assert(!terrain(Pos(4, 11)), "y out of bounds (4,11)")
      assert(!terrain(Pos(7, 5)), "x out of bounds (7,5)")
      assert(!terrain(Pos(7, 11)), "both out of bounds (7,11)")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
      assert(goal == Pos(4, 7))
    }
  }

  test("neighbors of startPos") {
    new Level1 {
      assert(startBlock.neighbors.toSet == List((Block(Pos(-1, 1), Pos(0, 1)), Up), (Block(Pos(1, 2), Pos(1, 3)), Right), (Block(Pos(2, 1), Pos(3, 1)), Down), (Block(Pos(1, -1), Pos(1, 0)), Left)).toSet, "invalid neighbors")
    }
  }

  test("legal neighbors of startPos") {
    new Level1 {
      assert(startBlock.legalNeighbors.toSet == List((Block(Pos(1, 2), Pos(1, 3)), Right), (Block(Pos(2, 1), Pos(3, 1)), Down)).toSet, "only right and down are legal")
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val res = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))

      val test = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet
      assert(test == res)

    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      val res = Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream
      val test = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))
      assert(test == res)
    }
  }


  	test("from on level without solution") {
    new LevelB {
      val res = Stream(
          (Block(Pos(0,0),Pos(0,0)),List()), 
          (Block(Pos(0,1),Pos(0,2)),List(Right)), 
          (Block(Pos(1,0),Pos(2,0)),List(Down)), 
          (Block(Pos(1,1),Pos(1,2)),List(Down, Right)), 
          (Block(Pos(1,1),Pos(2,1)),List(Right, Down)), 
          (Block(Pos(1,0),Pos(1,0)),List(Left, Down, Right)), 
          (Block(Pos(0,1),Pos(0,1)),List(Up, Right, Down)))
      assert(from(Stream((startBlock,List())),Set(startBlock))== res)
      }
    }
  

  test("from on a trivial level") {
    new LevelA {
      val res = from(initialStream, Set(startBlock))
      assert(res == Stream(
        (Block(Pos(0, 0), Pos(0, 0)), List()),
        (Block(Pos(1, 0), Pos(2, 0)), List(Down)),
        (Block(Pos(1, 1), Pos(2, 1)), List(Right, Down)),
        (Block(Pos(0, 1), Pos(0, 1)), List(Up, Right, Down))))
    }
  }
  
  
  test("solution on a trivial level") {
    new LevelA {
      assert(solution== List(Up, Right, Down).reverse)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
