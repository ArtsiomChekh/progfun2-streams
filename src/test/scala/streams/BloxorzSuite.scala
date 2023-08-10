package streams

import Bloxorz.*
import org.junit.Assert
import org.junit.Assert.{assertFalse, assertTrue}

class BloxorzSuite extends munit.FunSuite:
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */

    import Move.*

    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
      }

  trait Level1 extends SolutionChecker:
    /* terrain for level 1*/

    val level: String =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val level1: String =
      """ST
       * |oo
       * |oo""".stripMargin

    val testLevel1: Vector[Vector[Char]] = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))

    import Move.*

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)


  test("terrain function level 1 (10pts)") {
    new Level1:
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
  }

  test("terrain function testLevel1 (10pts)") {
    new Level1:
      assertTrue(terrainFunction(testLevel1)(Pos(1, 1)))
      assertFalse(terrainFunction(testLevel1)(Pos(1, 3)))
  }

  test("find char level 1 (10pts)") {
    new Level1:
      assertEquals(startPos, Pos(1, 1))
      assertEquals(goal, Pos(4, 7))
  }

  test("find char testLevel1 (10pts)") {
    new Level1:
      assertEquals(findChar('o', testLevel1), Pos(1, 0))
      assertNotEquals(findChar('o', testLevel1), Pos(0, 0))
      assertEquals(findChar('S', testLevel1), Pos(0, 0))
      assertEquals(findChar('T', testLevel1), Pos(0, 1))
  }


  test("optimal solution for level 1 (5pts)") {
    new Level1:
      assertEquals(solve(solution), Block(goal, goal))
  }


  test("optimal solution length for level 1 (5pts)") {
    new Level1:
      assertEquals(solution.length, optsolution.length)
  }

  test("Block.isStanding") {
    new Level1:
      val b1 = Block(Pos(0, 0), Pos(0, 0))
      val b2 = Block(Pos(0, 0), Pos(0, 1))
      assertTrue(b1.isStanding)
      assertFalse(b2.isStanding)
  }

  test("Block.isLegal") {
    new Level1:
      val b1 = Block(Pos(0, 0), Pos(0, 0))
      val b2 = Block(Pos(0, 0), Pos(0, 3))
      assertTrue(b1.isLegal)
      assertFalse(b2.isLegal)
  }

  test("Block.startBlock") {
    new Level1:
      val pos1 = Pos(1, 1)
      val pos2 = Pos(1, 1)
      val b = Block(pos1, pos2)
      assertTrue(startBlock == b)
  }

  test("Block.neghbors") {
    new Level1:
      val b = Block(Pos(0, 0), Pos(0, 0))
      assertEquals(b.neighbors.length, 4)
  }

  test("Block.legalNeghbors") {
    new Level1:
      val b = Block(Pos(1, 1), Pos(1, 1))
      assertEquals(b.legalNeighbors,
        List((Block(Pos(1, 2), Pos(1, 3)), Move.Right), (Block(Pos(2, 1), Pos(3, 1)), Move.Down)))
  }

  test("Solver.done") {
    new Level1:
      val b1 = Block(Pos(4, 7), Pos(4, 7))
      val b2 = Block(Pos(4, 6), Pos(4, 6))
      assert(done(b1))
      assert(!done(b2))
  }

  test("Solver.neighborsWithHistory") {
    new Level1:
      assertEquals(
        neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Move.Left, Move.Up)),
        LazyList((Block(Pos(1, 2), Pos(1, 3)), List(Move.Right, Move.Left, Move.Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Move.Down, Move.Left, Move.Up))))
  }

  test("Solver.newNeighborsOnly") {
    new Level1:
      assertEquals(newNeighborsOnly(

        LazyList(
          (Block(Pos(1, 2), Pos(1, 3)), List(Move.Right, Move.Left, Move.Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Move.Down, Move.Left, Move.Up))),
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))),

        LazyList(
          (Block(Pos(2, 1), Pos(3, 1)), List(Move.Down, Move.Left, Move.Up))))
  }

  test("Solver.from") {
    new Level1:
      val list = from(LazyList((startBlock, List())), Set(startBlock))

      assertEquals(
        list.take(1),

        LazyList((Block(Pos(1, 1), Pos(1, 1)), List()))
      )

      assertEquals(
        list.take(2),

        LazyList((Block(Pos(1, 1), Pos(1, 1)), List()),
          (Block(Pos(1, 2), Pos(1, 3)), List(Move.Right)))
      )
  }


  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds
