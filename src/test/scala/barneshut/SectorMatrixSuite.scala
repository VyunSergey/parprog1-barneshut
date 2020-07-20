package barneshut

import scala.{collection => coll}
import org.junit.Test

class SectorMatrixSuite extends BaseSuite {
  // test cases for sector matrix
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

  @Test def `'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96 (2pts)`: Unit = {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    (boundaries.minX = 1, boundaries.maxX = 97)
    (boundaries.minY = 1, boundaries.maxY = 97)

    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body

    checkSectorMatrixLength(sm, 1)
    checkSectorMatrix(sm, 2, 3, coll.Seq(body))
  }

  @Test def `'SectorMatrix.+=' should add 4 bodies: [(2,2), (6,2), (2,6), (6,6)] to the correct bucket of a sector matrix`: Unit = {
    val b1 = new Body(5, 2, 2, 0.1f, 0.1f)
    val b2 = new Body(5, 6, 2, 0.1f, 0.1f)
    val b3 = new Body(5, 2, 6, 0.1f, 0.1f)
    val b4 = new Body(5, 6, 6, 0.1f, 0.1f)

    val boundaries = new Boundaries()
    (boundaries.minX = 0, boundaries.maxX = 8)
    (boundaries.minY = 0, boundaries.maxY = 8)

    val sm = new SectorMatrix(boundaries, 2)
    sm += b1
    sm += b2
    sm += b3
    sm += b4

    checkSectorMatrixLength(sm, 4)
    checkSectorMatrix(sm, 0, 0, coll.Seq(b1))
    checkSectorMatrix(sm, 1, 0, coll.Seq(b2))
    checkSectorMatrix(sm, 0, 1, coll.Seq(b3))
    checkSectorMatrix(sm, 1, 1, coll.Seq(b4))
  }

  @Test def `'SectorMatrix.+=' should add 7 bodies: [(-5,-5), (-5,0), (0,-5), (0,0), (5,0), (0,5), (5,5)] to the correct bucket of a sector matrix`: Unit = {
    val b1 = new Body(1, -5, -5, 0.1f, 0.1f)
    val b2 = new Body(1, -5,  0, 0.1f, 0.1f)
    val b3 = new Body(1,  0, -5, 0.1f, 0.1f)
    val b4 = new Body(1,  0,  0, 0.1f, 0.1f)
    val b5 = new Body(1,  0,  5, 0.1f, 0.1f)
    val b6 = new Body(1,  5,  0, 0.1f, 0.1f)
    val b7 = new Body(1,  5,  5, 0.1f, 0.1f)

    val boundaries = new Boundaries()
    (boundaries.minX = -5, boundaries.maxX = 5)
    (boundaries.minY = -5, boundaries.maxY = 5)

    val sm = new SectorMatrix(boundaries, 4)
    sm += b1
    sm += b2
    sm += b3
    sm += b4
    sm += b5
    sm += b6
    sm += b7

    checkSectorMatrixLength(sm, 7)
    checkSectorMatrix(sm, 0, 0, coll.Seq(b1))
    checkSectorMatrix(sm, 0, 2, coll.Seq(b2))
    checkSectorMatrix(sm, 2, 0, coll.Seq(b3))
    checkSectorMatrix(sm, 2, 2, coll.Seq(b4))
    checkSectorMatrix(sm, 2, 3, coll.Seq(b5))
    checkSectorMatrix(sm, 3, 2, coll.Seq(b6))
    checkSectorMatrix(sm, 3, 3, coll.Seq(b7))
  }

  @Test def `'SectorMatrix combine' works with two SectorMatrix`: Unit = {
    val b1 = new Body(5, 2, 2, 0.1f, 0.1f)
    val b2 = new Body(5, 6, 2, 0.1f, 0.1f)
    val b3 = new Body(5, 2, 6, 0.1f, 0.1f)
    val b4 = new Body(5, 6, 6, 0.1f, 0.1f)

    val boundaries = new Boundaries()
    (boundaries.minX = 0, boundaries.maxX = 8)
    (boundaries.minY = 0, boundaries.maxY = 8)

    val sm1 = new SectorMatrix(boundaries, 2)
    sm1 += b1
    sm1 += b2
    val sm2 = new SectorMatrix(boundaries, 2)
    sm2 += b3
    sm2 += b4
    val sm = sm1.combine(sm2)

    checkSectorMatrixLength(sm, 4)
    checkSectorMatrix(sm, 0, 0, coll.Seq(b1))
    checkSectorMatrix(sm, 1, 0, coll.Seq(b2))
    checkSectorMatrix(sm, 0, 1, coll.Seq(b3))
    checkSectorMatrix(sm, 1, 1, coll.Seq(b4))
  }
}
