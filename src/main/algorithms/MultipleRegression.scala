/**
 * @author Havan
 */

package algorithms

import matrix.Matrix
import matrix.ColumnVector
import scala.math._

object MultipleRegression {
  // TODO: Implement this
  def getRegressionCoefficients(inputFeature: List[List[Double]], output: List[Double]) = {
    
  }
  
  /**
   * Takes a list of rows, 
   * where each row is a set of feature functions applied to the i'th input,
   * and returns a matrix of the same, with a column of 1's appended (for the intercept vector)
   * 
   */
  def getFeatureMatrix(inputFeatures: List[List[Double]]) = {
    new Matrix(inputFeatures).prepend(ColumnVector.unitVector(inputFeatures.length))
  }
  
  /**
   * Takes a list of output responses
   * And returns a column vector with the same data
   */
  def getReponseVector(output: List[Double]) = {
    new ColumnVector(output)
  }
  
  def regressionGradientDescent(
      featureMatrix: Matrix, 
      output: ColumnVector, 
      initialWeights: ColumnVector,
      stepSize: Double,
      tolerance: Double) = {
    
    def converged(gradientSumSquaresMag: Double) = {
      if (abs(gradientSumSquaresMag) < tolerance) true
      else false
    }
    
    // This uses matrix arithmetic!
    def error(weights: ColumnVector) = {
      val predictions = featureMatrix * weights      
      output - predictions
    }
    
    def RSS(weights: ColumnVector) = {
      val rss = -(featureMatrix.transpose * error(weights)) * 2
      // println(s"RSS for ${weights.mkString(",")}: ${rss.magnitude}")
      rss
    }
    
    def iterate(weights: ColumnVector, gradientSumSquaresMag: Double): ColumnVector = {
      if (!converged(gradientSumSquaresMag)) {
        val newWeights = weights - (RSS(weights) * stepSize)
        val newGradientSumSquares = RSS(newWeights)
        // println(newWeights.mkString(","))
        iterate(newWeights, newGradientSumSquares.magnitude)
      }
      else {
        weights
      }
    }
    
    iterate(initialWeights, RSS(initialWeights).magnitude)
  }
}