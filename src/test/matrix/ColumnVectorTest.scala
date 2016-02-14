/**
 * @author Havan
 */

package matrix

import org.scalatest.FlatSpec

class ColumnVectorTest extends FlatSpec{
  "A ColumnVector" should "be printable" in {
    val columnVec = new ColumnVector(List(1, 2, 10, 3))
    println(columnVec)
  }
  
  "A unit vector" should "have only 1's" in {
    val unit = ColumnVector.unitVector(10)
    unit.foreach(value => assert(value == 1))
  }
  
  "A zero vector" should "have only 0's" in {
    val zero = ColumnVector.zeroVector(10)
    zero.foreach(value => assert(value == 0))
  }
  
  "A ColumnVector" should "remain the same when added to zero vector" in {
    val columnVec = new ColumnVector(List(1, 2, 10, 3))
    assert(columnVec == (columnVec + ColumnVector.zeroVector(columnVec.length)))
  } 
}