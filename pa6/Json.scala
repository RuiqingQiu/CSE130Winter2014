
class Doc(val lines: List[String]) { 
  
  def width  = lines.map(_.length).padTo(1, 0).max
  def height = lines.length
  override def toString = lines.map("[" + _ + "]").mkString("\n")

  def widen(ls: List[String]) =
    ls.map(l => l + " " * (width - l.length))

  def hcatT(that: Doc) : Doc = { 
    var maxThisWordLength = 0
    this.lines.foreach(l =>
      if(l.length > maxThisWordLength)
        maxThisWordLength = l.length)
    var maxThatWordLength = 0
    that.lines.foreach(l =>
      if(l.length > maxThatWordLength)
        maxThatWordLength = l.length)
    //Make every element in the this to be the same length
    val newThis = this.lines.map(x => x.padTo(maxThisWordLength," ").mkString)
    //Make every element in the that to be the same length
    val newThat = that.lines.map(x => x.padTo(maxThatWordLength," ").mkString)

    //If this has more elements
    if(this.lines.length > that.lines.length){
      //Add enough elements of the max length so that it matches this
      val newThatList2 = newThat.padTo(this.lines.length, "".padTo(maxThatWordLength, " ").mkString)
      val result = 
        for ((x, y) <- (newThis zip newThatList2))
          yield x + y
      Doc(result)
    }
    else if(this.lines.length < that.lines.length){
      val newThisList2 = newThis.padTo(that.lines.length, "".padTo(maxThisWordLength, " ").mkString)
      val result = 
        for((x, y) <- (newThisList2 zip newThat))
          yield x + y  
      Doc(result)
    }
    else{
      val result = 
        for((x, y) <- (newThis zip newThat))
          yield x + y  
      Doc(result)
    }
  }

  def hcatB(that: Doc) : Doc = {
    var maxThisWordLength = 0
    this.lines.foreach(l =>
      if(l.length > maxThisWordLength)
        maxThisWordLength = l.length)
    var maxThatWordLength = 0
    that.lines.foreach(l =>
      if(l.length > maxThatWordLength)
        maxThatWordLength = l.length)
    //Make every element in the this to be the same length
    val newThis = this.lines.map(x => x.padTo(maxThisWordLength," ").mkString)
    //Make every element in the that to be the same length
    val newThat = that.lines.map(x => x.padTo(maxThatWordLength," ").mkString)

    //If this has more elements
    if(this.lines.length > that.lines.length){
      //Add enough elements of the max length so that it matches this
      val newThatList2 = newThat.padTo(this.lines.length, "".padTo(maxThatWordLength, " ").mkString).reverse
      val result = 
        for ((x, y) <- (newThis zip newThatList2))
          yield x + y
      Doc(result)
    }
    else if(this.lines.length < that.lines.length){
      val newThisList2 = newThis.padTo(that.lines.length, "".padTo(maxThisWordLength, " ").mkString).reverse
      val result = 
        for((x, y) <- (newThisList2 zip newThat))
          yield x + y  
      Doc(result)
    }
    else{
      val result = 
        for((x, y) <- (newThis zip newThat))
          yield x + y  
      Doc(result)
    } 
  } 

  def vcat(that: Doc) : Doc = {
    var maxLength = 0
    this.lines.foreach(l => 
      if (l.length > maxLength) 
        maxLength = l.length)
    that.lines.foreach(l =>
      if(l.length > maxLength)
        maxLength = l.length)
    val newList = this.lines.map(x => x.padTo(maxLength, " ").mkString)
    val newList1 = that.lines.map(x => x.padTo(maxLength, " ").mkString)
    Doc(newList:::newList1)
  }
  
}

object Doc {
  def apply(s: String)         = new Doc(List(s))
  def apply(xs: List[String])  = new Doc(xs)
  def apply(xs: String*)       = new Doc(xs.toList)
  def empty : Doc              = new Doc(List())
  def vcats(ds: List[Doc]):Doc = ds.foldLeft(empty)(_.vcat(_))
  
  def vcats(ds: List[Doc], start: Doc, sep: Doc, end: Doc): Doc =
    ds match {
      case Nil      => start hcatT end
      case d :: Nil => start hcatT d hcatB end
      case d :: _   => (start hcatT d) vcat (vcats(ds.tail.map(sep hcatT _)) hcatB end)
    }
  
  /**
   * Function: padBegin
   * Description: the input list xs padded with enough copies of x at
   * the beginning to make the result list have length n
   **/
  def padBegin[A](xs: List[A], n: Int, x:A): List[A] = {
    val difference = n - xs.length
    if(difference > 0)
      xs.reverse.padTo(n, x).reverse      
    else if(difference < 0)
      xs
    else
      xs
  }

  /**
   * Function: padEnd
   * Description: same as padBegin, pad everything at the end of the list
   **/
  def padEnd[A](xs: List[A], n: Int, x:A): List[A] = {
    val difference = n - xs.length
    if(difference > 0)
      xs.padTo(n, x)   
    else if (difference < 0)
      xs
    else
      xs
  }
}

/******************************************************************/
/*************** The Json TypeClass *******************************/
/******************************************************************/

sealed abstract class JVal 
case class JStr(s: String)            extends JVal
case class JNum(n: BigDecimal)        extends JVal
case class JObj(o: Map[String, JVal]) extends JVal
case class JArr(a: List[JVal])        extends JVal

object JVal {
  
  private def renderDocs(ds: List[Doc]) = 
    Doc.vcats(ds, Doc("{ "), Doc(", "), Doc(" }"))
 
  //Helper function for getting JArr
  private def renderDocsArr(ds: List[Doc]) =
    Doc.vcats(ds, Doc("[ "), Doc(", "), Doc(" ]"))
  /** 
   * Function: render
   * Description: Use the Doc formatting class to show formatted Json
   * String
   **/
  def render(jv: JVal) : Doc = { 
    //Match JVal to its cases
    jv match{
      case JStr(s) => Doc(s)
      case JNum(n) => Doc(n.toString)
      case JObj(m) => 
        var docList:List[Doc] = List()
          //Loop through elements in the map
          for (element <- m)
            //Update the list with the current element
            docList = docList :+ (Doc(element._1 + " : ") hcatT render(element._2)) 
        //RenderDocs to make the {} and ,
        renderDocs(docList)
      case JArr(a) =>
        var docList:List[Doc] = List()
        for(element <- a){
          docList = docList :+ render(element)
        }
        renderDocsArr(docList)
    }
  }

}


/******************************************************************/
/*************** The Json TypeClass *******************************/
/******************************************************************/

//http://marakana.com/s/scala_typeclasses,1117/index.html

trait Json { 
  def json: JVal
}

object JsonWriter {
  def write(v: JVal): String = 
    JVal.render(v).toString 
  def write[A <% Json](x: A): String = 
    write(x.json)
}

/******************************************************************/
/*************** Creating Json Instances **************************/
/******************************************************************/

object Json { 

  implicit def stringJson(s: String) = 
    new Json {
      def json: JVal = JStr(s)
    }

  implicit def intJson(i: Int) = 
    new Json { 
      def json: JVal = JNum(i)
    }

  implicit def tup2Json[A1 <% Json, A2 <% Json](p: (A1, A2)) =  
    new Json { 
      def json: JVal = JObj(Map( 
         "fst" -> p._1.json
       , "snd" -> p._2.json
      ))
    }

  implicit def tup3Json[A1 <% Json, A2 <% Json, A3 <% Json](p: (A1, A2, A3)) : Json = {  
    new Json {
      def json: JVal = JObj(Map(
        "fst" -> p._1.json
      , "snd" -> p._2.json
      , "thd" -> p._3.json
      ))
    }
  }

  implicit def listJson[A <% Json](xs: List[A]) : Json = { 
    new Json{
      def json: JVal = {
        var l1: List[JVal] = List()
        for(element <- xs)
          l1 = l1 :+ element.json
        JArr(l1)
      }
    }
  }


  implicit def arrJson[A <% Json](xs: Array[A]) : Json = { 
    new Json{
      def json: JVal = {
        var l1: List[JVal] = List()
        for(element <- xs)
          l1 = l1 :+ element.json
        JArr(l1)
      }
    }
  }

  implicit def mapJson[A <% Json](m: Map[String, A]) : Json = { 
    new Json{
      def json: JVal = {
        val map:Map[String, JVal] =
          for(element <- m)
            yield (element._1 -> element._2.json)
        JObj(map)
      }
    }
  }

}

/******************************************************************/
/********* Converting and Rendering Scala Values as Json **********/
/******************************************************************/

object JsonTest {
  import Json._
  val tup0 = (1, "cat")
  val tup1 = (tup0, tup0, tup0)
  val tup2 = (1, "cat", tup1)
  val tup  = (((1, (2, (3, (4, (5, "Nil")))))))
  val intl = List(("one", 1), ("two", 2), ("three", 3))
  val ints = Array(1, 2, 3, 4)
  val mm   = Map("mon" -> 10, "tue" -> 20, "wed" -> 30) 
  val lang = Map( "eng" -> List(("one", 1), ("two", 2), ("three", 3))
                , "spanish" -> List(("uno", 1), ("dos", 2), ("tres", 3))
                , "french" -> List(("un", 1) , ("deux", 2), ("trois", 3)))
  
  val trip = (ints, mm, lang)

  lazy val jvals0 = tup0.json
  lazy val jvals1 = tup1.json
  lazy val jvals2 = tup2.json
  lazy val jvals3 = tup.json
  lazy val jvals4 = intl.json
  lazy val jvals5 = ints.json
  lazy val jvals6 = mm.json
  lazy val jvals7 = lang.json
  lazy val jvals8 = trip.json

  
  // Real JVals 
  val jvReals = List( 
      JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))
    
    , JObj(Map("fst" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))
              ,"snd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))
              ,"thd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))))

    , JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat"), "thd" -> JObj(Map("fst" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat"))), "snd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat"))), "thd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))))))

    , JObj(Map("fst" -> JNum(1), "snd" -> JObj(Map("fst" -> JNum(2), "snd" -> JObj(Map("fst" -> JNum(3), "snd" -> JObj(Map("fst" -> JNum(4), "snd" -> JObj(Map("fst" -> JNum(5), "snd" -> JStr("Nil")))))))))))

    , JArr(List(JObj(Map("fst" -> JStr("one"), "snd" -> JNum(1)))
              , JObj(Map("fst" -> JStr("two"), "snd" -> JNum(2)))
              , JObj(Map("fst" -> JStr("three"), "snd" -> JNum(3)))))
                  
    , JArr(List(JNum(1), JNum(2), JNum(3), JNum(4)))

    , JObj(Map("mon" -> JNum(10), "tue" -> JNum(20), "wed" -> JNum(30)))

    , JObj(Map("eng" -> JArr(List(JObj(Map("fst" -> JStr("one"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("two"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("three"), "snd" -> JNum(3))))), "spanish" -> JArr(List(JObj(Map("fst" -> JStr("uno"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("dos"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("tres"), "snd" -> JNum(3))))), "french" -> JArr(List(JObj(Map("fst" -> JStr("un"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("deux"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("trois"), "snd" -> JNum(3)))))))


    , JObj(Map("fst" -> JArr(List(JNum(1), JNum(2), JNum(3), JNum(4))), "snd" -> JObj(Map("mon" -> JNum(10), "tue" -> JNum(20), "wed" -> JNum(30))), "thd" -> JObj(Map("eng" -> JArr(List(JObj(Map("fst" -> JStr("one"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("two"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("three"), "snd" -> JNum(3))))), "spanish" -> JArr(List(JObj(Map("fst" -> JStr("uno"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("dos"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("tres"), "snd" -> JNum(3))))), "french" -> JArr(List(JObj(Map("fst" -> JStr("un"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("deux"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("trois"), "snd" -> JNum(3)))))))))
  )
   
}

/*************************************************************************/
/***************** Property Based Testing ********************************/
/*************************************************************************/

import org.scalacheck._
import org.scalacheck.Prop._
import Gen._
import Arbitrary.arbitrary

object DocProperties extends Properties("Doc") {
 
  def genChar: Gen[Char] = 
    Gen.choose(97, 122).map(_.toChar)

  def genString: Gen[String] = 
    Gen.containerOf[List, Char](genChar).map(_.mkString(""))
  
  implicit def arbDoc: Arbitrary[Doc] =
    Arbitrary { 
      for (ls <- Gen.containerOf[List, String](genString)) 
      yield Doc(ls) 
    }

  val prop_hcatT_width = forAll((d1: Doc, d2: Doc) => 
    (d1 hcatT d2).width == d1.width + d2.width
  )
  
  val prop_hcatT_height = forAll((d1: Doc, d2: Doc) => 
    (d1 hcatT d2).height == (d1.height max d2.height)
  )

  val prop_hcatB_width = forAll((d1: Doc, d2: Doc) => 
    (d1 hcatB d2).width == d1.width + d2.width
  )

  val prop_hcatB_height = forAll((d1: Doc, d2: Doc) => 
    (d1 hcatB d2).height == (d1.height max d2.height)
  )
 
  val prop_vcat_width = forAll((d1: Doc, d2: Doc) => 
    (d1 vcat d2).width == (d1.width max d2.width)
  )

  val prop_vcat_height = forAll((d1: Doc, d2: Doc) => 
    (d1 vcat d2).height == d1.height + d2.height
  )

  // run all properties with:
  // $ scala DocProperties
  property("prop_hcatB_height") = prop_hcatB_height
  property("prop_hcatB_width")  = prop_hcatB_width
  property("prop_hcatT_height") = prop_hcatT_height
  property("prop_hcatT_width")  = prop_hcatT_width
  property("prop_vcat_height")  = prop_vcat_height
  property("prop_vcat_width")   = prop_vcat_width


}  
// vim: set ts=2 sw=2 et:
