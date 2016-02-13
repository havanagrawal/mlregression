/**
 * @author Havan
 */

package algorithms

import scala.math.pow

object LinearRegression {
  
  /**
   * Computes the slope and intercept for given input(features) and output (response)
   * 
   * This uses the closed-form solution for calculating the co-efficients
   */
  def simpleLinearRegression(inputFeature: List[Double], outputFeature: List[Double]) = {
    val n = inputFeature.size
    val summationY = outputFeature.sum
    val summationX = inputFeature.sum
    val summationX2 = inputFeature.map(x => x * x).sum
    val summationXY = inputFeature.zip(outputFeature).map{
      case (x, y) => x * y
    }.sum
        
    val w1Num = n * summationXY - (summationX * summationY)
    val w1Den = n * summationX2 - (summationX * summationX)
    
    val w1 = w1Num/w1Den
    val w0 = (summationY - w1 * summationX) / n
    
    Parameters(w0, w1)
  }
  
  /**
   * Accepts a column of input features, and a slope and intercept pair
   * And returns a column of predictions
   */
  def getRegressionPredictions(inputFeature: List[Double], intercept: Double, slope: Double) = {
    inputFeature.map(x => slope*x + intercept)
  }
  
  /**
   * Accepts a column of input features, and expected output
   * along with slope and intercept, and returns the sum of squares of error
   */
  def getResidualSumOfSquares(
      inputFeature: List[Double], 
      output: List[Double], 
      intercept: Double, 
      slope: Double) = {
    
    val predictions = getRegressionPredictions(inputFeature, intercept, slope)
    predictions.zip(output).map{
      case (predicted, actual) => pow(predicted - actual, 2) 
    }.sum
  }
  
  /**
   * Given a column of output responses, slope and intercept,
   * applies the linear equation inverse to get estimated input
   * 
   * This is possible only since the relationship is between two variables,
   * and can be easily inversed
   */
  def inverseRegressionPredictions(output: List[Double], intercept: Double, slope: Double) = {
    output.map(op => (op - intercept)/slope)
  }
  
  case class Parameters(intercept: Double, slope: Double)
}