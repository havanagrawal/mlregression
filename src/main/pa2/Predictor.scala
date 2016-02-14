/**
 * @author Havan
 */

package pa2

import dataio.CSVReader
import algorithms.MultipleRegression._
import matrix.ColumnVector

object Predictor extends App {
  val trainingDataFileReader = new CSVReader("res/kc_house_train_data.csv")
  val testDataFileReader = new CSVReader("res/kc_house_test_data.csv")

  val trainingData = trainingDataFileReader.readAll().toList
  val testData = testDataFileReader.readAll().toList
  
  val prices = trainingData.map(record => record("price").toDouble)
  
  /* Explanation:
  * Consider x = List(1,2,3,4,5). Giving this to getFeatureMatrix as List(x) will result in a 1 x 6 matrix:
  * 1 1 2 3 4 5. What we want is to treat each of these numbers in the list as an observation.
  * Thus, x.map(obs => List(obs)) gives us the desired result of a 5 x 1 matrix.
  * getFeatureMatrix will append a column of 1's to this, resulting in:
  * 1 1
  * 1 2
  * 1 3
  * 1 4
  * 1 5
  * Which is what we want.
  * 
  * In case there are multiple input features, simply use zip followed by map(toList)
  */
  val sqft_living = trainingData.map(record => record("sqft_living").toDouble).map(obs => List(obs))
  
  val featureMatrix = getFeatureMatrix(sqft_living)
  val responseVector = getReponseVector(prices)
  val initialWeights = new ColumnVector(List(-47000, 1))
  
  /*
   *  Quiz Question: 
   *      What is the value of the weight for sqft_living,
   *      the second element of ‘simple_weights’ (rounded to 1 decimal place)?
   */
  val weights = regressionGradientDescent(featureMatrix, responseVector, initialWeights, 7e-12, 2.5e7)
  println(weights.mkString(", "))
}