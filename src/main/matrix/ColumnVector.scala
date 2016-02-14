/**
 * @author Havan
 */

package matrix

/*
 * TODO: Make this class indexable
 */
class ColumnVector(val vec: Seq[Double]) {
  lazy val length = vec.size
  lazy val magnitude = math.sqrt(this.map(x => x * x).vec.sum)
  
  def +(that: ColumnVector) = {
    new ColumnVector(this.vec.zip(that.vec).map {
      case (x, y) => x + y
    })
  }

  def -(that: ColumnVector) = {
    this + (-that)
  }

  def map(f: => (Double => Double)) = {
    new ColumnVector(this.vec.map(f))
  }

  def foreach(f: => Double => Unit) = {
    this.vec.foreach(f)
  }

  /**
   * Multiplies the two ColumnVectors element by element,
   * adds the results,
   * and returns a scalar value
   */
  def dotProduct(that: ColumnVector) = {
    this.vec.zip(that.vec).map {
      case (x, y) => x * y
    }.sum
  }

  def *(scalar: Double) = {
    this.map(x => x * scalar)
  }

  def unary_-() = {
    this.map(x => -x)
  }

  // TODO: Return a matrix
  def zip(that: ColumnVector) = {
    throw new UnsupportedOperationException("This is yet to be implemented. Sorry!")
  }

  def ==(that: ColumnVector) = {
    this.vec == that.vec
  }

  override def toString() = {
    vec.toString()
  }
}

object ColumnVector {
  def unitVector(length: Int) = {
    new ColumnVector(Seq.fill(length)(1))
  }

  def zeroVector(length: Int) = {
    new ColumnVector(Seq.fill(length)(0))
  }
}