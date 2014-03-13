import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter
import java.io.File
case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {
  /**
   * Function name: apply
   * Description: takes a string and returns an instance of the case 
   * class, which has all the fields
   **/
  def apply(line: String) : Entry = {
    //Separate the string based on ":"
    val splitedArray = line.split(":")
    new Entry(splitedArray(0),splitedArray(1),splitedArray(2).toInt,
       splitedArray(3).toInt,
       splitedArray(4),splitedArray(5),splitedArray(6))
  }
}

object Crack {
  /**
   * Function name: transformReverse
   * Description: takes a string and return an iterator over the original
   * string and reversal of the original string.
   **/
  def transformReverse(w: String) : Iterator[String] = {
    return Iterator(w, w.reverse)
  }

  /**
   * Function name: transformCapitalize
   * Description: takes a string and returns an iterator over all 
   * possible ways to capitalize the input string
   **/
  def transformCapitalize(w: String) : Iterator[String] = {
    if (w == "") {Iterator(w)}
    else
      for (c <- Iterator(w.substring(0,1).toUpperCase, w.substring(0,1));
           s <- transformCapitalize(w.substring(1)))
        yield(c + s)
  }

  /**
   * Function name: transformDigits
   * Description: return an iteraot over all possible ways to replace
   * letters with similar looking digits according to the mappings in 
   * the table.
   **/
  def transformDigits(w:String) : Iterator[String] = {
    if (w == "") {Iterator(w)}
    else{
      val firstLetter = w.substring(0,1)
      for (c <- ((firstLetter.toLowerCase) match{
            case "o" => Iterator(firstLetter,"0")
            case "z" => Iterator(firstLetter,"2")
            case "a" => Iterator(firstLetter,"4")
            case "b" => Iterator(firstLetter,"6","8")
            case "g" => Iterator(firstLetter,"9")
            case "q" => Iterator(firstLetter,"9")
            case "i" => Iterator(firstLetter,"1")
            case "l" => Iterator(firstLetter,"1")
            case "e" => Iterator(firstLetter,"3")
            case "s" => Iterator(firstLetter,"5")
            case "t" => Iterator(firstLetter,"7")
            case _ => Iterator(firstLetter)
          });
          s <- transformDigits(w.substring(1)))
        yield(c+s)
    }
  }

  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1

  /**
   * Function name: apply
   * Description: takes a password file, a file with a list of words 
   * and a string name for output file. It will attempt to crack as many 
   * of the passwords in the password file as possible and write them
   * into the output file
   **/
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    //Reading the content of the password file
    val pwdFileContent = scala.io.Source.fromFile(pwdFile).getLines()
    val wordFileContent = scala.io.Source.fromFile(wordsFile).getLines()
    //Open out file to write to
    val writer = new PrintWriter(new File(outFile))
    //A set to contain all the password entries
    var entries = Set[Entry]()
    //Add Entry to the set
    for(pwdLine <- pwdFileContent){
      entries = entries + Entry(pwdLine)
    }
    //Make a copy to avoid concurrent modification
    var entriesCopy = entries.&(entries)
    //First check, only the words with no transformation
    val threads = 
    for(e <- entries toList)
      yield new Thread(){
        override def run(){
          val account = e.account
          val password = e.password
          val words = candidateWords(wordsFile)
          var found = false
          for(word <- words){
            if(!found){
              if(checkPassword(word, password)){
                println(account + "=" + word + "\n")
                writer.write(account + "=" + word + "\n")
                writer.flush()
                entriesCopy = entriesCopy - e
                found = true
              }
            }
          }
        }
      }
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())
    println("No transformation DONE") 
    var entriesCopy2 = entriesCopy.&(entriesCopy)
    //Second pass of applying reverse 
    val threads2 = 
    for(e <- entriesCopy toList)
      yield new Thread()
      {
        override def run() 
        {   
          //Reset found to be false
          var found = false
          val account = e.account
          val password = e.password
          var words = candidateWords(wordsFile)
          //This loop is for only applying reverse
          for(w <- words) {
            if(!found){
              var skipOriginal = true
              val reverseIter = transformReverse(w)
              for(i <- reverseIter){
                //Skip the first time since it's already checked
                if(!skipOriginal){
                  if(!found){
                    if(checkPassword(i, password)){
                      println(account + "=" + i + "\n")
                      writer.write(account + "=" + i + "\n")
                      writer.flush()
                      entriesCopy2 = entriesCopy2 - e
                      found = true
                    }
                  }
                }
                //Enter here the first time, set the flag false
                else{
                  skipOriginal = false
                }
              }
            }
          }//End of word loop
        }
      }
    threads2.foreach(t => t.start())
    threads2.foreach(t => t.join())
    println("reverse check DONE") 

    entriesCopy = entriesCopy2.&(entriesCopy2)
    //Fourth pass, applying digits
    val threads4 = 
    for(e <- entriesCopy toList)
      yield new Thread()
      {
        override def run()
        {
          var found = false
          val account = e.account
          val password = e.password
          //This loop is for only applying digits
          var words = candidateWords(wordsFile)
          for(w <- words){
            if(!found){
              val digitIter = transformDigits(w)
              for(i <- digitIter){
                if(!found){
                  if(checkPassword(i, password)){
                    println(account + "=" + i + "\n")
                    writer.write(account + "=" + i + "\n")
                    writer.flush()
                    entriesCopy2 = entriesCopy2 - e
                    found = true
                  }
                }
              }
            }
          }
        }
      }
    threads4.foreach(t => t.start())
    threads4.foreach(t => t.join())
    println("Digit check DONE") 
    
    entriesCopy = entriesCopy2.&(entriesCopy2)
    //Third pareach(t => t.start())

    entriesCopy = entriesCopy2.&(entriesCopy2)
    //Applying various combination of transformation
    val threads5 = 
    for(e <- entriesCopy toList)
      yield new Thread()
      {
        override def run()
        {
          println("5 running")
          var found = false
          val account = e.account
          val password = e.password
          var words = candidateWords(wordsFile)
          //This loop is for applying reverse and capital
          for(w <- words){
            if(!found){
              val reverseIter = transformReverse(w)
              for(r <- reverseIter){
                val digitReverseIter = transformDigits(r)
                for(d <- digitReverseIter){
                  if(!found){
                    if(checkPassword(d, password)){
                      println(account + "=" + d + "\n")
                      writer.write(account + "=" + d + "\n")
                      writer.flush()
                      entriesCopy2 = entriesCopy2 - e
                      found = true
                    }
                  }
                }
              }
            }
          }//End of for statement
        }
      }
    threads5.foreach(t => t.start())
    //threads5.foreach(t => t.join())
    println("reverse and digit combine DONE")
    entriesCopy = entriesCopy2.&(entriesCopy2)
    //applying capitalize, slow!
    val threads3 = 
    for(e <- entriesCopy toList)
      yield new Thread()
      {
        override def run()
        {
          println("3 running")
          var found = false
          val account = e.account
          val password = e.password
          //This loop is for only applying capitalize
          var words = candidateWords(wordsFile)
          for(w <- words){
            if(!found){
              val capitalIter = transformCapitalize(w)
              for(i <- capitalIter){
                if(!found){
                  if(checkPassword(i,password)){
                    println(account + "=" + i + "\n")
                    writer.write(account + "=" + i + "\n")
                    writer.flush()
                    entriesCopy2 = entriesCopy2 - e
                    found = true
                  }
                }
              }
            }
          }//End of word loop
        }
      }
    threads3.foreach(t => t.start())
    //threads3.foreach(t => t.join())
    //this thread involves capital with other, therefore takes a 
    //really long time.
    entriesCopy = entriesCopy2.&(entriesCopy2)
    val threads6 = 
    for(e <- entriesCopy toList)
      yield new Thread()
      {
        override def run()
        {
          println("6 is running")
          var found = false
          val account = e.account
          val password = e.password
          //This loop is for applying digit and capitalize
          var words = candidateWords(wordsFile)
          for(w <- words)
            {
            if(!found){
              val digitIter = transformDigits(w)
              for(d <- digitIter){
                val capDigitIter = transformCapitalize(d)
                for(c <- capDigitIter){
                  if(!found){
                    if(checkPassword(c, password)){
                      println(account + "=" + c + "\n")
                      writer.write(account + "=" + c + "\n")
                      writer.flush()
                      found = true
                    }
                  }
                }
              }
            }
          }

          //This loop is for applying reverse and digit
          if(!found){
            words = candidateWords(wordsFile)
            for(w <- words){
              if(!found){
                val reverseIter = transformReverse(w)
                for(r <- reverseIter){
                  val capReverseIter = transformCapitalize(r)
                  for(c <- capReverseIter){
                    if(!found){
                      if(checkPassword(c, password)){
                        println(account + "=" + c + "\n")
                        writer.write(account + "=" + c + "\n")
                        writer.flush()
                        found = true
                      }
                    }
                  }
                }
              }
            }//End of for statement
          }//End of if statement

          if(!found){
            words = candidateWords(wordsFile)
            for(w <- words)
            {
              if(!found){
                val reverseIter = transformReverse(w)
                for(r <- reverseIter){
                  val capReverseIter = transformCapitalize(r)
                  for(c <- capReverseIter){
                    val digitCapReverseIter = transformDigits(c)
                    for(d <- digitCapReverseIter)
                    {
                      if(!found){
                        if(checkPassword(d, password)){
                          println(account + "=" + d + "\n")
                          writer.write(account + "=" + d + "\n")
                          writer.flush()
                          found = true
                        }
                      }
                    }
                  }
                }
              }
            }
          }//End of if
        }//End of def run
      }//End of making threads
    threads6.foreach(t => t.start())
    threads6.foreach(t => t.join())
    threads5.foreach(t => t.join())
    threads3.foreach(t => t.join())
  }


def main(args: Array[String]) = { 
  println("Begin: Cracking Passwords")
  apply(args(0), args(1), args(2))
  println("Done: Cracking Passwords")
}
}

// vim: set ts=2 sw=2 et:

