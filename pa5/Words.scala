import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  /**
    * Function name: apply
    * Argument Type: file: String
    * Return Type: Iterator[String]
    * Description: this function is use to create word. And it will
    * convert all strings in the file to lower case
    */
  def apply(file: String) : Iterator[String] = { 
    val fileContent = scala.io.Source.fromFile(file).getLines()
    fileContent.map(_.toLowerCase)
  }
  
  /**
   * Function name: groupFreq
   * Description: takes an iterator and a grouping function f that
   * converts type A value into their B groups
   **/
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = {
    var map : HashMap[B, Int] = HashMap()
    for (x <- xs) {
      map.get(f(x)) match {
        case Some(n) => map += (f(x) -> (n + 1))
        case None => map += (f(x) -> 1)
      }
    }
    return map
  }
  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  /**
   * Function: sizeFreq
   * Description: this function computes a HashMap that maps integer
   * word-length to the number of words of that length
   **/
  def sizeFreq(file: String): HashMap[Int, Int] = {
    val fileContents = scala.io.Source.fromFile(file).getLines()
    //getting all the length for each word in the file 
    val lengths = for (line <- fileContents) yield line.size
    //Grouper function
    val grouper = (x: Int) => x
    groupFreq(lengths, grouper)
  }

  /**
   * Function: charFreq
   * Description: this function computers a HashMap that maps 
   * each Char c to the number of times  c appears accross 
   * throughout the entrie file
   **/
  def charFreq(file: String): HashMap[Char, Int] = 
  {
    //Read in the fileContent of the file
    val fileContents = scala.io.Source.fromFile(file).getLines()
    //For each line in the file, get its Char and yield an Char Iterator 
    val chars   =  for(line <- fileContents; c <- line) yield c.toLower
    //Group function takes an Char and return the same Char
    val grouper = (x: Char) => x
    groupFreq(chars, grouper) 
  }

  /**
   * Function: wordsOfSize
   * Description: returns an iterator over all words in file that 
   * have length equal to size
   **/
  def wordsOfSize(file: String, size: Int) : Iterator[String] = {
    val fileContents = scala.io.Source.fromFile(file).getLines()
    //Using the filter function to filter out anything string 
    //that is not the same size of the argument
    fileContents.filter((line:String) => line.size == size)
  }

  /**
   * Function: wordsWithAllVowels
   * Descrption: returns an iterator over all words in file that
   * contain all five vowels
   **/
  def wordsWithAllVowels(file: String): Iterator[String] = {
    val fileContents = scala.io.Source.fromFile(file).getLines()
    //Filter out any words that doesn't contain all vowels
    fileContents.map(_.toLowerCase).filter((line:String) => 
      (line.contains('a') && line.contains('e') &&
       line.contains('i') && line.contains('u') &&
       line.contains('o')))
  }

  /**
   * Function: wordsWithNoVowels
   * Description: return an iterator over all the words in file 
   * that do not contain any vowel
   **/
  def wordsWithNoVowels(file: String): Iterator[String] = { 
    val fileContents = scala.io.Source.fromFile(file).getLines()
    fileContents.map(_.toLowerCase).filter((line:String) => 
      !(line.contains('a') || line.contains('e') ||
        line.contains('i') || line.contains('u') ||
        line.contains('o')))
  }

  /**
   * Function: wordsMatchingRegexp
   * Description: loads the words from the file which match
   * the regular expression.
   **/
  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = {
    val fileContents = scala.io.Source.fromFile(file).getLines()
    fileContents.map(_.toLowerCase).filter(
      (line:String) => line.matches(re.toString))
  }
}

// vim: set ts=2 sw=2 et:

