/**
 * @author Havan
 */
package pa1

import scala.io.Source
import java.util.Arrays
import algorithms.LinearRegression

object Predictor extends App {
  
  import LinearRegression._
  
  val trainingDataInputFile = "res/kc_house_train_data.csv"
  val testDataInputFile = "res/kc_house_test_data.csv"
  
  // Consider storing this as a case class, or a map
  val trainingData = Source.fromFile(trainingDataInputFile).getLines.map{line =>
    line.split(",").map(_.trim)
  }.toList
  
  val testData = Source.fromFile(testDataInputFile).getLines.map{line =>
    line.split(",").map(_.trim)
  }.toList
  
  // Printing data for debugging purposes
  /*trainingData.take(30).foreach{line =>
    line.foreach(col => print(col + "\t"))
    println
  }*/
  
  // View data column headers
  // trainingData.head.foreach(feature => print(s"$feature\t"))
  
  // tail to remove header from the column
  val pricesTraining = trainingData.tail.map(record => record(2).toDouble)
  val sqftLivingTraining = trainingData.tail.map(record => record(5).toDouble)
  val bedroomsTraining = trainingData.tail.map(record => record(3).toDouble) 
  
  val pricesTest = testData.tail.map(record => record(2).toDouble)
  val sqftLivingTest = testData.tail.map(record => record(5).toDouble)
  val bedroomsTest = testData.tail.map(record => record(3).toDouble)
  
  val sqftLivingToPriceCoeffs = simpleLinearRegression(sqftLivingTraining, pricesTraining)
  val bedroomToPriceCoeffs = simpleLinearRegression(bedroomsTraining, pricesTraining)
  
  val sqftIntercept = sqftLivingToPriceCoeffs.intercept
  val sqftSlope = sqftLivingToPriceCoeffs.slope
  
  val bedroomIntercept = bedroomToPriceCoeffs.intercept
  val bedroomSlope = bedroomToPriceCoeffs.slope
  
  /*
   * Quiz Question: 
   *    Using your Slope and Intercept from (4),
   *    What is the predicted price for a house with 2650 sqft?
   */
  val sqft = 2650
  val prediction = LinearRegression.getRegressionPredictions(List(sqft), sqftIntercept, sqftSlope)
  println(s"6. Cost of house with $sqft sqft is $prediction")
  
  /*
   * Quiz Question:
   *    According to this function and the slope and intercept from (4)
   *    What is the RSS for the simple linear regression using squarefeet to predict prices on TRAINING data?
   */  
  val rssSqftLivingTraining = getResidualSumOfSquares(sqftLivingTraining, pricesTraining, sqftIntercept, sqftSlope)
   
  println(s"8. RSS for Sqft Living -> Prices Model On Training Dataset:\t$rssSqftLivingTraining")
  
  /*
   *  Quiz Question: 
   *      According to this function and the regression slope and intercept from (3) 
   *      what is the estimated square-feet for a house costing $800,000?
   */
  val cost = 800000
  val estimatedInput = inverseRegressionPredictions(List(cost), sqftIntercept, sqftSlope)
  println(s"10. The estimated sqft of a house costing $cost is $estimatedInput")
  
  /*
   * Quiz Question: 
   *    Which model (square feet or bedrooms) has lowest RSS on TEST data? 
   *    Think about why this might be the case.
   */
  
  val rssSqftLivingTest = getResidualSumOfSquares(sqftLivingTest, pricesTest, sqftIntercept, sqftSlope)
  val rssBedroomTest = getResidualSumOfSquares(bedroomsTest, pricesTest, bedroomIntercept, bedroomSlope)
  println("13.")
  println(s"\tRSS for Sqft Living -> Prices Model On Test Dataset:\t$rssSqftLivingTest")
  println(s"\tRSS for Bedrooms -> Prices Model On Test Dataset:\t$rssBedroomTest")
  
}