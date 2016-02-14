/**
 * @author Havan
 */

package matrix

/**
 * Represents a matrix of Double
 *
 * TODO: Make this more generic
 */
class Matrix(val mat: Seq[Seq[Double]]) {
  val rows = mat.size
  val cols = mat.head.size

  /**
   * Takes a sequence of column vectors
   * and appends them side by side to create a matrix.
   *
   * This simply fixes the row number, and then takes each column vector,
   * and creates one row. This is done as many times as the column vectors length
   *
   * Algorithm:
   *  for i <- 0...lengthOfColumnVector - 1
   *    row = []
   *    for j <- noOfColumnVectors
   *      row = row + jth columnVector[i]
   *    return row
   *
   * The dummy implicit is required to avoid compiler problems with type erasure
   * TODO: Dimension checks
   */
  def this(colVectors: Seq[ColumnVector])(implicit d: DummyImplicit) = {
    this(
      (0 until colVectors.length).map { rowIndex =>
        colVectors.map { x =>
          x.vec(rowIndex)
        }.toSeq
      }.toSeq)
  }

  /** 
   *  Returns a new matrix that is the transpose of this matrix
   *  
   *  It first converts each row into a column vector,
   *  and then constructs the new matrix with these column vectors
   */
  def transpose() = {
    new Matrix(mat.map { row =>
      new ColumnVector(row)
    })
  }

  /**
   * Multiplies an n x d matrix with a d-vector
   *
   * TODO: Dimension checks
   */
  def *(vector: ColumnVector) = {
    new ColumnVector(mat.map { row =>
      row.zip(vector.vec).map {
        case (x, y) => x * y
      }.sum
    })
  }
  
  /**
   * Prepends a column vector to this matrix
   * 
   * TODO: Dimension checks
   */
  def prepend(column: ColumnVector) = {
    (0 until column.length).map{i =>
      Seq(column.vec(i)) ++ mat(i)
    }
  }
  
  /**
   * Checks if this matrix can be represented as a column vector.
   * i.e. Returns true iff the dimensions of this matrix are n x 1,
   * false otherwise.
   */
  def isColumnVector() = {
    mat.forall { _.length == 1 }
  }
  
  /**
   * Returns an Option, in an attempt to convert this matrix to a column vector.
   * Returns Some[ColumnVector] if it was converted successfully, else None
   */
  def toColumnVector() = {
    if (isColumnVector) {
      Some(new ColumnVector(mat.map(row => row(0))))
    }
    else {
      None
    }
  }
}