/**
 * @author Havan
 */

package matrix

/*
 * TODO: Make this class indexable
 */
class ColumnVector(val vec: Seq[Double]) {
  val length = vec.size

  def +(that: ColumnVector) = {
    new ColumnVector(this.vec.zip(that.vec).map {
      case (x, y) => x + y
    })
  }
  
  def -(that: ColumnVector) = {
    this + that.map(x => -x)
  }
  
  def map(f: => (Double => Double)) = {
    new ColumnVector(this.vec.map(f))
  }
  
  def foreach(f: => Double => Unit) = {
    this.vec.foreach(f)
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