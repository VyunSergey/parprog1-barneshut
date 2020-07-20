package barneshut

import org.junit.Assert.fail
import org.junit.Test

import scala.collection._

class QuadSuite extends BaseSuite {
  import FloatOps._
  // test cases for quad tree

  @Test def `Empty: center of mass should be the center of the cell`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  @Test def `Empty: mass should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  @Test def `Empty: total should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  @Test def `Empty.insert(b) should return a Leaf with only that body (2pts)`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)

    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail(s"Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  @Test def `Leaf with 1 body`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  @Test def `Leaf.insert(b) should return a Fork with plus that body`: Unit = {
    val b1 = new Body(113f, 18f, 26f, 0f, 0f)
    val b2 = new Body(107f, 12.5f, 24f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 10f, Seq(b1))
    val inserted = quad.insert(b2)

    inserted match {
      case fork: Fork =>
        assert(fork.centerX == 17.5f, s"centerX ${fork.centerX} should be 17.5f")
        assert(fork.centerY == 27.5f, s"centerY ${fork.centerY} should be 27.5f")
        assert(fork.size == 10f, s"size ${fork.size} should be 10f")
        assert(fork.mass ~= 220f, s"mass ${fork.mass} should be 220f")
        assert(fork.massX ~= 15.325f, s"massX ${fork.massX} should be 15.325f")
        assert(fork.massY ~= 25.0273f, s"massY ${fork.massY} should be 25.0273f")
        assert(fork.total == 2, s"total ${fork.total} should be 2")

        fork.nw match {
          case leaf: Leaf =>
            assert(leaf.centerX == 15f, s"Fork.nw centerX ${leaf.centerX} should be 15f")
            assert(leaf.centerY == 25f, s"Fork.nw centerY ${leaf.centerY} should be 25f")
            assert(leaf.size == 5f, s"Fork.nw size ${leaf.size} should be 5f")
            assert(leaf.mass ~= 107f, s"Fork.nw mass ${leaf.mass} should be 107f")
            assert(leaf.massX ~= 12.5f, s"Fork.nw massX ${leaf.massX} should be 12.5f")
            assert(leaf.massY ~= 24f, s"Fork.nw massY ${leaf.massY} should be 24f")
            assert(leaf.total == 1, s"Fork.nw total ${leaf.total} should be 1")
            assert(leaf.bodies == Seq(b2), s"Fork.nw bodies ${leaf.bodies} should contains $b2")
          case _ =>
            fail(s"Fork.nw should have returned a Leaf, was ${fork.nw}")
        }

        fork.ne match {
          case leaf: Leaf =>
            assert(leaf.centerX == 20f, s"Fork.nw centerX ${leaf.centerX} should be 20f")
            assert(leaf.centerY == 25f, s"Fork.nw centerY ${leaf.centerY} should be 25f")
            assert(leaf.size == 5f, s"Fork.nw size ${leaf.size} should be 5f")
            assert(leaf.mass ~= 113f, s"Fork.nw mass ${leaf.mass} should be 113f")
            assert(leaf.massX ~= 18f, s"Fork.nw massX ${leaf.massX} should be 18f")
            assert(leaf.massY ~= 26f, s"Fork.nw massY ${leaf.massY} should be 26f")
            assert(leaf.total == 1, s"Fork.nw total ${leaf.total} should be 1")
            assert(leaf.bodies == Seq(b1), s"Fork.nw bodies ${leaf.bodies} should contains $b1")
          case _ =>
            fail(s"Fork.ne should have returned a Leaf, was ${fork.ne}")
        }

      case _ =>
        fail(s"Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  @Test def `Fork with 3 empty quadrants and 1 leaf (nw)`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"centerX ${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"centerY ${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"mass ${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"massX ${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"massY ${quad.massY} should be 26f")
    assert(quad.total == 1, s"total ${quad.total} should be 1")
  }

  @Test def `Fork.insert(b) should return a Fork `: Unit = {
    val nw = Empty( 7.5f, 17.5f, 5f)
    val ne = Empty(12.5f, 17.5f, 5f)
    val sw = Empty( 7.5f, 12.5f, 5f)
    val se = Empty(12.5f, 12.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    val b1 = new Body(10f, 10f, 16f, 0f, 0f)
    val b2 = new Body(20f,  7f, 15f, 0f, 0f)
    val b3 = new Body(30f, 10f, 12f, 0f, 0f)
    val b4 = new Body(40f, 14f, 15f, 0f, 0f)

    val inserted = quad.insert(b1).insert(b2).insert(b3).insert(b4)

    inserted match {
      case fork: Fork =>
        assert(fork.centerX == 10f, s"Fork centerX ${fork.centerX} should be 10f")
        assert(fork.centerY == 15f, s"Fork centerY ${fork.centerY} should be 15f")
        assert(fork.mass ~= 100f, s"Fork mass ${fork.mass} should be 100f")
        assert(fork.massX ~= 11f, s"Fork massX ${fork.massX} should be 11f")
        assert(fork.massY ~= 14.2f, s"Fork massY ${fork.massY} should be 14.2f")
        assert(fork.total == 4, s"Fork total ${fork.total} should be 4")

        fork.nw match {
          case leaf: Leaf =>
            assert(leaf.centerX == 7.5f, s"Fork.nw centerX ${leaf.centerX} should be 7.5f")
            assert(leaf.centerY == 17.5f, s"Fork.nw centerY ${leaf.centerY} should be 17.5f")
            assert(leaf.size == 5f, s"Fork.nw size ${leaf.size} should be 5f")
            assert(leaf.mass ~= 10f, s"Fork.nw mass ${leaf.mass} should be 10f")
            assert(leaf.massX ~= 10f, s"Fork.nw massX ${leaf.massX} should be 10f")
            assert(leaf.massY ~= 16f, s"Fork.nw massY ${leaf.massY} should be 16f")
            assert(leaf.total == 1, s"Fork.nw total ${leaf.total} should be 1")
            assert(leaf.bodies == Seq(b1), s"Fork.nw bodies ${leaf.bodies} should contains $b1")
          case _ =>
            fail(s"Fork.nw should have returned a Leaf, was ${fork.nw}")
        }

        fork.sw match {
          case leaf: Leaf =>
            assert(leaf.centerX == 7.5f, s"Fork.sw centerX ${leaf.centerX} should be 7.5f")
            assert(leaf.centerY == 12.5f, s"Fork.sw centerY ${leaf.centerY} should be 12.5f")
            assert(leaf.size == 5f, s"Fork.sw size ${leaf.size} should be 5f")
            assert(leaf.mass ~= 20f, s"Fork.sw mass ${leaf.mass} should be 20f")
            assert(leaf.massX ~= 7f, s"Fork.sw massX ${leaf.massX} should be 7f")
            assert(leaf.massY ~= 15f, s"Fork.sw massY ${leaf.massY} should be 15f")
            assert(leaf.total == 1, s"Fork.sw total ${leaf.total} should be 1")
            assert(leaf.bodies == Seq(b2), s"Fork.sw bodies ${leaf.bodies} should contains $b2")
          case _ =>
            fail(s"Fork.sw should have returned a Leaf, was ${fork.sw}")
        }

        fork.se match {
          case leaf: Leaf =>
            assert(leaf.centerX == 12.5f, s"Fork.se centerX ${leaf.centerX} should be 12.5f")
            assert(leaf.centerY == 12.5f, s"Fork.se centerY ${leaf.centerY} should be 12.5f")
            assert(leaf.size == 5f, s"Fork.se size ${leaf.size} should be 5f")
            assert(leaf.mass ~= 30f, s"Fork.se mass ${leaf.mass} should be 30f")
            assert(leaf.massX ~= 10f, s"Fork.se massX ${leaf.massX} should be 10f")
            assert(leaf.massY ~= 12f, s"Fork.se massY ${leaf.massY} should be 12f")
            assert(leaf.total == 1, s"Fork.se total ${leaf.total} should be 1")
            assert(leaf.bodies == Seq(b3), s"Fork.se bodies ${leaf.bodies} should contains $b3")
          case _ =>
            fail(s"Fork.sw should have returned a Leaf, was ${fork.se}")
        }

        fork.ne match {
          case leaf: Leaf =>
            assert(leaf.centerX == 12.5f, s"Fork.ne centerX ${leaf.centerX} should be 12.5f")
            assert(leaf.centerY == 17.5f, s"Fork.ne centerY ${leaf.centerY} should be 17.5f")
            assert(leaf.size == 5f, s"Fork.ne size ${leaf.size} should be 5f")
            assert(leaf.mass ~= 40f, s"Fork.ne mass ${leaf.mass} should be 40f")
            assert(leaf.massX ~= 14f, s"Fork.ne massX ${leaf.massX} should be 14f")
            assert(leaf.massY ~= 15f, s"Fork.ne massY ${leaf.massY} should be 15f")
            assert(leaf.total == 1, s"Fork.ne total ${leaf.total} should be 1")
            assert(leaf.bodies == Seq(b4), s"Fork.ne bodies ${leaf.bodies} should contains $b4")
          case _ =>
            fail(s"Fork.nw should have returned a Leaf, was ${fork.ne}")
        }

      case _ =>
        fail(s"Fork.insert() should have returned a Fork, was $inserted")
    }
  }
}
