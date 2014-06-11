package bert.pickling

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalatest.prop.PropertyChecks
import java.util.Arrays
import bert.Bert

class BertPickleFormatTest extends WordSpec with Matchers with Checkers with PropertyChecks {
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
    "produce a Bert compatible pickled list" in {
      import scala.pickling._
      import bert.pickling._
      
      val pickle = List(1,2,3,4).pickle
      val result = Bert.fromBert[List[Int]](pickle.value).get
      
      result should be(List(1,2,3,4))
    }
  }
  
  "Default pickling" should {
    "unpickle a binary pickled list" in {
      import scala.pickling._
      import json._
      
      val pickle = List(1,2,3,4).pickle
      val result = pickle.unpickle[List[Int]]
      
      result should be(List(1,2,3,4))
    }
  }
}