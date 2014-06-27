package bert.pickling

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalatest.prop.PropertyChecks
import java.util.Arrays
import bert.Bert

class BertPickleFormatSpec extends WordSpec with Matchers with Checkers with PropertyChecks {
  "BertPickleFormat" should {
    "unpickle a pickled list" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = List(1,2,3,4).pickle
      val result = pickle.unpickle[List[Int]]
      
      result should be(List(1,2,3,4))
    }
    "unpickle a pickled Int" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = 12.pickle
      val result = pickle.unpickle[Int]
      
      result should be(12)
    }
    "unpickle a pickled Long" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = 12L.pickle
      val result = pickle.unpickle[Long]
      
      result should be(12)
    }
    "unpickle a pickled short" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = 12.toShort.pickle
      val result = pickle.unpickle[Short]
      
      result should be(12)
    }
    "unpickle a pickled byte" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = 12.toByte.pickle
      val result = pickle.unpickle[Byte]
      
      result should be(12)
    }
    "produce a Bert compatible pickled list" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = List(1,2,3,4).pickle
      val result = Bert.fromBert[List[Int]](pickle.value).get
      
      result should be(List(1,2,3,4))
    }
    "unpickle a pickled string" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = "hello scala".pickle
      val result = pickle.unpickle[String]
      
      result should be("hello scala")
    }
    "unpickle a pickled double" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = -12.3.pickle
      val result = pickle.unpickle[Double]
      
      result should be(-12.3)
    }
    "unpickle null" in {
      import scala.pickling._
      import bert.pickling._
      
      val ref: Object = null
      val pickle = ref.pickle
      val result = pickle.unpickle[Object]
      
      result should be(null)
    }
    "unpickle an int array" in {
      import scala.pickling._
      import bert.pickling._

      val arr = Array(1,2,3)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Int]]
      
      result should be(Array(1,2,3))
    }
  }
  
  "Default pickling" should {
    "unpickle a binary pickled list" in {
      import scala.pickling._
      import json._
      
      val pickle = 12.toShort.pickle
      val result = pickle.unpickle[Short]
      
      result should be(12.toShort)
    }
  }
}