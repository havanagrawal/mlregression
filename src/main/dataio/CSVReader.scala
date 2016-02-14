/**
 * @author Havan
 */

package dataio

import java.nio.file.{ Paths, Files }
import java.io.FileNotFoundException
import scala.io.Source

/**
 * Contains methods that allow reading a CSV file with headers
 *
 * TODO:
 *    Make this generic enough to accept type parameters for each column,
 *    And cast the input into those types
 */
class CSVReader(val filepath: String) {
  if (!Files.exists(Paths.get(filepath)))
    throw new FileNotFoundException("The CSV file you specified does not exist")

  private val lines = Source.fromFile(filepath).getLines

  val headers = lines.next().split(",").map(_.trim).toList

  /**
   * Reads the next record in the CSV data
   *
   * This consumes one record from the CSV file, in that it behaves like an iterator.
   * Calling readNext() on an empty file will result in an Exception
   * 
   * It is advisable to call hasNext to check if the file has reached the end.
   */
  def readNext() = {
    val values = lines.next().split(",").map(_.trim)
    headers.zip(values).toMap
  }

  /**
   * Reads all of the records in the CSV file.
   *
   * Calling this after having called readNext() is inadvisable,
   * as reading the file consumes data. Calling readAll after readNext will result
   * in reading the rest of the file, not the entire file from beginning to end.
   */
  def readAll() = lines.map { record =>
    headers.zip(record.split(",").map(_.trim)).toMap
  }

  /**
   * Checks if the reader has another record to read.
   */
  def hasNext = lines.hasNext
}