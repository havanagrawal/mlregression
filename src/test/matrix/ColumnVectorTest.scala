/**
 * @author Havan
 */

package matrix

import org.scalatest.FlatSpec

class ColumnVectorTest extends FlatSpec {

  import ColumnVector._

  "A unit vector" should "have only 1's" in {
    val unit = unitVector(10)
    unit.foreach(value => assert(value == 1))
  }

  "A zero vector" should "have only 0's" in {
    val zero = zeroVector(10)
    zero.foreach(value => assert(value == 0))
  }
  
  "A ColumnVector" should "be printable" in {
    val columnVec = new ColumnVector(List(1, 2, 10, 3))
    println(columnVec)
  }

  it should "remain the same when added to zero vector" in {
    val columnVec = new ColumnVector(List(1, 2, 10, 3))
    assert(columnVec == (columnVec + zeroVector(columnVec.length)))
  }

  it should "remain the same when multiplied by 1" in {
    val columnVec = new ColumnVector(List(1, 2, 10, 3))
    assert(columnVec == columnVec * 1)
  }
  
  it should "result in zero when multiplied with a zero vector" in {
    val columnVec = new ColumnVector(List(1, 2, 10, 3))
    val len = columnVec.length
    val zero = zeroVector(len)
    assert(0 == (columnVec.dotProduct(zero)))
  }

  it should "result in a zero vector when multiplied with 0" in {
    val columnVec = new ColumnVector(List(1, 2, 10, 3))
    val len = columnVec.length
    val zero = zeroVector(len)
    assert(zero == columnVec * 0)
  }

  
}