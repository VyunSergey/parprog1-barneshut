import java.util.concurrent._
import scala.{collection => coll}
import scala.util.DynamicVariable
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX: Float = Float.MaxValue
    var minY: Float = Float.MaxValue

    var maxX: Float = Float.MinValue
    var maxY: Float = Float.MinValue

    def width: Float = maxX - minX
    def height: Float = maxY - minY

    def size: Float = math.max(width, height)

    def centerX: Float = minX + width / 2
    def centerY: Float = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"

    def nearest(x: Float, y: Float): (Float, Float) = {
      val nx = if(x > maxX) maxX else if(x < minX) minX else x
      val ny = if(y > maxY) maxY else if(y < minY) minY else y
      (nx, ny)
    }
  }

  sealed abstract class Quad extends QuadInterface {
    /**
     * `massX` and `massY` represent the center of mass of the bodies in the respective cell
     * */
    def massX: Float
    def massY: Float
    /**
     * `mass` is the total mass of bodies in that cell
     * */
    def mass: Float
    /**
     * `centerX` and `centerY` are the coordinates of the center of the cell
     * */
    def centerX: Float
    def centerY: Float
    /**
     * `size` is the length of the side of the cell
     * */
    def size: Float
    /**
     * `total` is the total number of bodies in the cell
     * */
    def total: Int
    /**
     * `insert` creates a new quadtree which additionally contains the body b,
     * and covers the same area in space as the original quadtree
     * */
    def insert(b: Body): Quad
  }

  sealed abstract class Position
  case object NorthWest extends Position
  case object NorthEast extends Position
  case object SouthWest extends Position
  case object SouthEast extends Position

  implicit class QuadExtensions(quad: Quad) {
    def show: String =
      s"Quad(centerX: ${quad.centerX}, centerY: ${quad.centerY}, size: ${quad.size}," +
        s" total: ${quad.total}, massX: ${quad.massX}, massY: ${quad.massY}, mass: ${quad.mass})"

    def contains(b: Body)(position: Position): Boolean = {
      val radius: Float = quad.size / 2
      position match {
        case NorthWest =>
          (quad.centerX - radius <= b.x && b.x <= quad.centerX + radius) &&
          (quad.centerY - radius <  b.y && b.y <= quad.centerY + radius)
        case NorthEast =>
          (quad.centerX - radius <  b.x && b.x <= quad.centerX + radius) &&
          (quad.centerY - radius <= b.y && b.y <= quad.centerY + radius)
        case SouthWest =>
          (quad.centerX - radius <= b.x && b.x <  quad.centerX + radius) &&
          (quad.centerY - radius <= b.y && b.y <= quad.centerY + radius)
        case SouthEast =>
          (quad.centerX - radius <= b.x && b.x <= quad.centerX + radius) &&
          (quad.centerY - radius <= b.y && b.y <  quad.centerY + radius)
      }
    }
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val centerX: Float = (nw.centerX + ne.centerX + sw.centerX + se.centerX) / 4
    val centerY: Float = (nw.centerY + ne.centerY + sw.centerY + se.centerY) / 4
    val size: Float = (nw.size + ne.size + sw.size + se.size) / 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass

    val massX: Float =
      if (mass == 0) centerX
      else (nw.massX * nw.mass + ne.massX * ne.mass + sw.massX * sw.mass + se.massX * se.mass) / mass

    val massY: Float =
      if (mass == 0) centerY
      else (nw.massY * nw.mass + ne.massY * ne.mass + sw.massY * sw.mass + se.massY * se.mass) / mass

    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if (nw.contains(b)(NorthWest)) Fork(nw.insert(b), ne, sw, se)
      else if (ne.contains(b)(NorthEast)) Fork(nw, ne.insert(b), sw, se)
      else if (sw.contains(b)(SouthWest)) Fork(nw, ne, sw.insert(b), se)
      else if (se.contains(b)(SouthEast)) Fork(nw, ne, sw, se.insert(b))
      else throw
        new IllegalArgumentException(s"Body $b does not contains in Quads:\n${List(nw, ne, sw, se).mkString("\n")}")
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: coll.Seq[Body]) extends Quad {
    val mass: Float = bodies.map(_.mass).sum
    val (massX: Float, massY: Float) = (bodies.map(b => b.x * b.mass).sum / mass, bodies.map(b => b.y * b.mass).sum / mass)
    val total: Int = bodies.length

    def insert(b: Body): Quad =
      if (size > minimumSize) {
        val quad: Float = size / 4
        val newSize = size / 2
        val fork = Fork(
          Empty(centerX - quad, centerY - quad, newSize),
          Empty(centerX + quad, centerY - quad, newSize),
          Empty(centerX - quad, centerY + quad, newSize),
          Empty(centerX + quad, centerY + quad, newSize)
        )
        (bodies :+ b).foldLeft(fork)((f, b) => f.insert(b))
      }
      else {
        Leaf(centerX, centerY, size, bodies :+ b)
      }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  /**
   * `mass` is body mass
   * `x` and `y` are the coordinates of the body
   * `xspeed` and `yspeed` represent the velocity of the body
   * */
  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {
    override def toString: String = s"Body(mass: $mass, x: $x, y: $y, xspeed: $xspeed, yspeed: $yspeed)"

    override def equals(obj: Any): Boolean = {
      val that: Body = obj.asInstanceOf[Body]
      mass == that.mass && x == that.x && y == that.y && xspeed == that.xspeed && yspeed == that.yspeed
    }

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = {
        (quad: Quad) match {
          case _: Empty =>
          // no force
          case leaf: Leaf =>
            // add force contribution of each body by calling addForce
            // addForce(leaf.mass, leaf.massX, leaf.massY)
            leaf.bodies.foreach(b => addForce(b.mass, b.x, b.y))
          case fork: Fork =>
            // see if node is far enough from the body,
            // or recursion is needed
            if (fork.size / distance(fork.massX, fork.massY, x, y) < theta) {
              addForce(fork.mass, fork.massX, fork.massY)
            }
            else {
              traverse(fork.nw)
              traverse(fork.ne)
              traverse(fork.sw)
              traverse(fork.se)
            }
        }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) extends SectorMatrixInterface {
    val sectorSize: Float = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- matrix.indices) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val (x, y) = boundaries.nearest(b.x, b.y)
      val tmpX = ((x - boundaries.minX) / sectorSize).toInt
      val tmpY = ((y - boundaries.minY) / sectorSize).toInt
      val indexX = if (tmpX < sectorPrecision) tmpX else sectorPrecision - 1
      val indexY = if (tmpY < sectorPrecision) tmpY else sectorPrecision - 1
      apply(indexX, indexY) += b
      this
    }

    def apply(x: Int, y: Int): ConcBuffer[Body] = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- matrix.indices) matrix(i) = matrix(i).combine(that.matrix(i))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          val emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR)
              parallel(
                quad(x, y, nspan, nAchievedParallelism),
                quad(x + nspan, y, nspan, nAchievedParallelism),
                quad(x, y + nspan, nspan, nAchievedParallelism),
                quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
              )
            else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear(): Unit = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        System.currentTimeMillis() - startTime
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: $totalTime ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString: String = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString "\n"
    }
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute: T = body
      }
      Thread.currentThread match {
        case _: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
}
