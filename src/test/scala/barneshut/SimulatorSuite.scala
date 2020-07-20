package barneshut

import scala.{collection => coll}
import org.junit.Test

class SimulatorSuite extends BaseSuite {
  // test cases for simulator
  def checkSectorMatrixLength(sectorMatrix: SectorMatrix, expected: Int): Unit = {
    val actual: Int = sectorMatrix.matrix.map(_.size).sum
    val message = s"SectorMatrix should contains $expected bodies but got $actual"
    assert(actual == expected, message)
  }

  def checkSectorMatrix(sectorMatrix: SectorMatrix, x: Int, y: Int, expected: coll.Seq[Body]): Unit = {
    val actual: coll.Seq[Body] = sectorMatrix(x, y).toIndexedSeq
    val message = s"SectorMatrix($x, $y) should contains bodies: $expected but got $actual"
    assert(actual.size == expected.size && actual.containsSlice(expected), message)
  }

  def checkBoundaries(boundaries: Boundaries, minX: Int, maxX: Int, minY: Int, maxY: Int): Unit = {
    assert(boundaries.minX == minX, s"Boundaries.minX should be $minX but got ${boundaries.minX}")
    assert(boundaries.maxX == maxX, s"Boundaries.maxX should be $maxX but got ${boundaries.maxX}")
    assert(boundaries.minY == minY, s"Boundaries.minY should be $minY but got ${boundaries.minY}")
    assert(boundaries.maxY == maxY, s"Boundaries.maxY should be $maxY but got ${boundaries.maxY}")
  }

  @Test def `updateBoundaries should work correctly`: Unit = {
    val body = new Body(5, 1000, -1000, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    (boundaries.minX = 0, boundaries.maxX = 10)
    (boundaries.minY = 0, boundaries.maxY = 10)

    val model = new SimulationModel
    model.initialize(1, "two-galaxies", 0)
    model.timeStats.clear()
    val simulator = new Simulator(model.taskSupport, model.timeStats)

    val newBoundaries = simulator.updateBoundaries(boundaries, body)
    checkBoundaries(newBoundaries, 0, 1000, -1000, 10)
  }

  @Test def `mergeBoundaries should work correctly`: Unit = {
    val bnd1 = new Boundaries()
    (bnd1.minX = -1000, bnd1.maxX = 0)
    (bnd1.minY = 0, bnd1.maxY = 1000)

    val bnd2 = new Boundaries()
    (bnd2.minX = 0, bnd2.maxX = 1000)
    (bnd2.minY = -1000, bnd2.maxY = 0)

    val model = new SimulationModel
    model.initialize(1, "two-galaxies", 0)
    model.timeStats.clear()
    val simulator = new Simulator(model.taskSupport, model.timeStats)

    val newBoundaries = simulator.mergeBoundaries(bnd1, bnd2)
    checkBoundaries(newBoundaries, -1000, 1000, -1000, 1000)
  }

  @Test def `computeBoundaries should work correctly`: Unit = {
    val b1 = new Body(5, -1000, 0, 0f, 0f)
    val b2 = new Body(5, 0, 1000, 0f, 0f)
    val b3 = new Body(5, 1000, 0, 0f, 0f)
    val b4 = new Body(5, 0, -1000, 0f, 0f)
    val bodies: coll.Seq[Body] = coll.Seq[Body](b1, b2, b3, b4)

    val model = new SimulationModel
    model.initialize(1, "two-galaxies", 0)
    model.timeStats.clear()
    val simulator = new Simulator(model.taskSupport, model.timeStats)

    val newBoundaries = simulator.computeBoundaries(bodies)
    checkBoundaries(newBoundaries, -1000, 1000, -1000, 1000)
  }

  @Test def `computeSectorMatrix should work correctly`: Unit = {
    val b11 = new Body(5, -1000, -1000, 0f, 0f)
    val b10 = new Body(5, -1000, 0, 0f, 0f)
    val b01 = new Body(5, 0, -1000, 0f, 0f)
    val b00 = new Body(5, 0, 0, 0f, 0f)
    val b02 = new Body(5, 0, 1000, 0f, 0f)
    val b20 = new Body(5, 1000, 0, 0f, 0f)
    val b22 = new Body(5, 1000, 1000, 0f, 0f)
    val bodies: coll.Seq[Body] = coll.Seq[Body](b11, b10, b01, b00, b02, b20, b22)

    val boundaries = new Boundaries()
    (boundaries.minX = -1000, boundaries.maxX = 1000)
    (boundaries.minY = -1000, boundaries.maxY = 1000)

    val model = new SimulationModel
    model.initialize(1, "two-galaxies", 0)
    model.timeStats.clear()
    val simulator = new Simulator(model.taskSupport, model.timeStats)

    val sm = simulator.computeSectorMatrix(bodies, boundaries)
    checkBoundaries(sm.boundaries, -1000, 1000, -1000, 1000)
    checkSectorMatrixLength(sm, 7)
    checkSectorMatrix(sm, 0, 0, coll.Seq(b11))
    checkSectorMatrix(sm, 0, 4, coll.Seq(b10))
    checkSectorMatrix(sm, 4, 0, coll.Seq(b01))
    checkSectorMatrix(sm, 4, 4, coll.Seq(b00))
    checkSectorMatrix(sm, 4, 7, coll.Seq(b02))
    checkSectorMatrix(sm, 7, 4, coll.Seq(b20))
    checkSectorMatrix(sm, 7, 7, coll.Seq(b22))
  }
}
