package barneshut

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.Seq

class BodySuite extends BaseSuite {
  import FloatOps._
  // test cases for Body

  @Test def `Body.updated should do nothing for Empty quad trees`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assertEquals(0f, body.xspeed, precisionThreshold)
    assertEquals(0f, body.yspeed, precisionThreshold)
  }

  @Test def `Body.updated should take bodies in a Leaf into account (2pts)`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))
    val body = b.updated(quad)

    assert(body.xspeed ~= 12.587037f, s"Body xspeed should be 12.587037f but got ${body.xspeed}")
    assert(body.yspeed ~= 0.015557117f, s"Body yspeed should be 0.015557117f but got ${body.yspeed}")
  }

  @Test def `Body.updated should take bodies in a Fork as center of mass`: Unit = {
    val b1 = new Body(10f, 10f, 16f, 0f, 0f)
    val b2 = new Body(20f,  7f, 15f, 0f, 0f)
    val b3 = new Body(30f, 10f, 12f, 0f, 0f)
    val b4 = new Body(40f, 14f, 15f, 0f, 0f)
    val b = new Body(100f, 1000f, 1000f, 12.5f, 7.12f)

    val quad = Fork(
      Leaf( 7.5f, 17.5f, 5f, Seq(b1)),
      Leaf(12.5f, 17.5f, 5f, Seq(b2)),
      Leaf( 7.5f, 12.5f, 5f, Seq(b3)),
      Leaf(12.5f, 12.5f, 5f, Seq(b4))
    )

    val body = b.updated(quad)

    assert(body.xspeed ~= 12.5f, s"Body xspeed should be 12.5f but got ${body.xspeed}")
    assert(body.yspeed ~= 7.12f, s"Body yspeed should be 7.12f but got ${body.yspeed}")
  }

  @Test def `Body.updated should take bodies in a Fork deep nesting`: Unit = {
    val b1 = new Body(10f, 10f, 16f, 0f, 0f)
    val b2 = new Body(20f,  7f, 15f, 0f, 0f)
    val b3 = new Body(30f, 10f, 12f, 0f, 0f)
    val b4 = new Body(40f, 14f, 15f, 0f, 0f)
    val b = new Body(100f, 10f, 15f, 12.5f, 7.12f)

    val quad = Fork(
      Leaf( 7.5f, 17.5f, 5f, Seq(b1)),
      Leaf(12.5f, 17.5f, 5f, Seq(b2)),
      Leaf( 7.5f, 12.5f, 5f, Seq(b3)),
      Leaf(12.5f, 12.5f, 5f, Seq(b4))
    )

    val body = b.updated(quad)

    assert(body.xspeed ~= 12.77778f, s"Body xspeed should be 12.77778f but got ${body.xspeed}")
    assert(body.yspeed ~= 3.786667f, s"Body yspeed should be 3.786667f but got ${body.yspeed}")
  }
}
