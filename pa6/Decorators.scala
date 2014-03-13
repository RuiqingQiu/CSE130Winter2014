import scala.collection.mutable.HashMap

object Decorators {
    
  object profile { 
    
    private var cm: Map[String, Int] =  Map().withDefault(_ => 0)

    def count(name: String) = 
      cm(name)
   
    def reset(name: String) = 
      cm += name -> 0

    def apply[A, B](name: String)(f: A => B) = new Function1[A, B] {
      def apply(x: A) = {
        val n = cm(name)
        cm += name -> (1 + n)
        f(x)
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////// 

  object trace {
    
    // You may add more fields here
    //A depth variable to keep track of the indentation
    var depth = 0

    def apply[A, B](name: String)(f: A => B) : Function1[A, B] = new Function1[A, B] {

      // You may add more fields here
      def apply (x: A): B = {
        try{
          //Create a string has depth number of |
          val pipe = "".padTo(depth, "| ").mkString
          depth = depth + 1
          //Print out the input value
          println(pipe + ",- " + name + "(" + x + ")")
          val value:B = f(x)
          //Finish call the function, decrement the depth
          depth = depth - 1
          //Print out the result
          println(pipe + "`- " + value)
          return value
        }
        catch{
          //Catch the result and decrement the result, throw the exception
          case e: Exception => { depth = depth -1; throw(e)}
        }
      }
    }
  } 
  
  ////////////////////////////////////////////////////////////////////////////// 
  
  object memo {
    // You may add more fields here
    def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {
      //Using a hashmap to keep track of argument and result
      var cache = new HashMap[A, Either[B, Throwable]]()
      // You may add more fields here
      def apply (x: A): B = {
        //If the result has been calculated
        if(cache.contains(x))
          cache(x) match{
            case Left(x) => x
            case Right(x) => throw(x)
          }
        //If not, then calculate it
        else{
          try{
            val result = f(x)
            cache += (x -> Left(result))
            result
          }
          catch{
            case e: Exception => { cache += (x -> Right(e)); throw(e)}
          }
        }
      } 
    }
  }
}
