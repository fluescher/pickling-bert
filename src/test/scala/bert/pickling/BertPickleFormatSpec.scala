package bert.pickling

import bert.Bert
import bert.pickling.TestData._
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalatest.{Matchers, WordSpec}

class BertPickleFormatSpec extends WordSpec with Matchers with Checkers with PropertyChecks {
  "BertPickleFormat for primitive types" should {
    import bert.pickling._

    import scala.pickling._

    "unpickle a pickled Int" in {
      val pickle = 12.pickle
      val result = pickle.unpickle[Int]

      result should be(12)
    }
    "unpickle a pickled Long" in {
      val pickle = 12L.pickle
      val result = pickle.unpickle[Long]

      result should be(12)
    }
    "unpickle a pickled short" in {
      val pickle = 12.toShort.pickle
      val result = pickle.unpickle[Short]

      result should be(12)
    }
    "unpickle a pickled byte" in {
      val pickle = 12.toByte.pickle
      val result = pickle.unpickle[Byte]

      result should be(12)
    }
    "unpickle a pickled char" in {
      val pickle = '@'.pickle
      val result = pickle.unpickle[Char]

      result should be('@')
    }
    "unpickle a pickled boolean" in {
      val pickle = true.pickle
      val result = pickle.unpickle[Boolean]
      result should be(true)

      val pickle2 = false.pickle
      val result2 = pickle2.unpickle[Boolean]
      result2 should be(false)
    }
    "unpickle a pickled double" in {
      val pickle = -12.3.pickle
      val result = pickle.unpickle[Double]

      result should be(-12.3)
    }
    "unpickle a pickled float" in {
      val pickle = -12.3f.pickle
      val result = pickle.unpickle[Float]

      result should be(-12.3f)
    }
    "unpickle a pickled short after an int" in {
      val pickle1 = 12.pickle
      val result1 = pickle1.unpickle[Int]
      result1 should be(12)

      val pickle2 = 12.toShort.pickle
      val result2 = pickle2.unpickle[Short]

      result2 should be(12)
    }
  }
  "BertPickleFormat for complex types" should {
    import bert.pickling._

    import scala.pickling._

    "unpickle a pickled list of primitives" in {
      val pickle = List(1, 2, 3, 4).pickle
      val result = pickle.unpickle[List[Int]]

      result should be(List(1, 2, 3, 4))
    }
    "unpickle a pickled list of case classes" in {
      val pickle = List(BlobberTest("flo", 1), BlobberTest("asdf", 2), BlobberTest("fdeds", 3)).pickle
      val result = pickle.unpickle[List[BlobberTest]]

      result should be(List(BlobberTest("flo", 1), BlobberTest("asdf", 2), BlobberTest("fdeds", 3)))
    }
    "unpickle a pickled string" in {
      val pickle = "hello scala".pickle
      val result = pickle.unpickle[String]

      result should be("hello scala")
    }
    "unpickle null" in {
      val ref: Object = null
      val pickle = ref.pickle
      val result = pickle.unpickle[Object]

      result should be(null)
    }
    "unpicke a simple case class" in {
      val blobber = BlobberTest("Blobber", 13)
      val pickle = blobber.pickle
      val result = pickle.unpickle[BlobberTest]

      result should be(blobber)
    }
    "unpickle a simple tuple" in {
      val pickle = (1,"test").pickle
      val result = pickle.unpickle[(Int,String)]

      result should be((1,"test"))
    }
    "unpickle a complex tuple" in {
      val pickle = (1,"test",("test", 1), 2).pickle
      val result = pickle.unpickle[(Int,String,(String,Int), Int)]

      result should be((1,"test", ("test", 1), 2))
    }
    "unpickle a tuple containing references" in {
      val test = "test"
      val pickle = (1,test,(test, 1), 2).pickle
      val result = pickle.unpickle[(Int,String,(String,Int), Int)]

      result should be((1,test, (test, 1), 2))
    }
  }

  "BertPickleFormat for arrays" should {
    import bert.pickling._

    import scala.pickling._

    "unpickle an byte array" in {
      val arr = Array(1.toByte, 2.toByte, 3.toByte)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Byte]]

      result should be(Array(1.toByte, 2.toByte, 3.toByte))
    }
    "unpickle an char array" in {
      val arr = Array('a', 'b', 'c')
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Char]]

      result should be(Array('a', 'b', 'c'))
    }
    "unpickle an short array" in {
      val arr = Array(-45.toShort, 123.toShort, 12.toShort)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Short]]

      result should be(Array(-45.toShort, 123.toShort, 12.toShort))
    }
    "unpickle an int array" in {
      val arr = Array(1, 2, 3)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Int]]

      result should be(Array(1, 2, 3))
    }
    "unpickle a long array" in {
      val arr = Array(12L, -156L, 444L, 12L)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Long]]

      result should be(Array(12L, -156L, 444L, 12L))
    }
    "unpickle a float array" in {
      val arr = Array(1.2f, -15.6f, 4.44f, 1.2f)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Float]]

      result should be(Array(1.2f, -15.6f, 4.44f, 1.2f))
    }
    "unpickle a double array" in {
      val arr = Array(1.2, -15.6, 4.44, 1.2)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Double]]

      result should be(Array(1.2, -15.6, 4.44, 1.2))
    }
    "unpickle a boolean array" in {
      val arr = Array(true, false, false, true)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Boolean]]

      result should be(Array(true, false, false, true))
    }
    "unpickle a array of a complex type" in {
      val arr = Array(BlobberTest("asdf", 2), BlobberTest("blobber", -2))
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[BlobberTest]]

      result should be(Array(BlobberTest("asdf", 2), BlobberTest("blobber", -2)))
    }
    "unpickle a array of a complex nested type" in {
      val arr = Array((1,"test",("test", 1), 2), (1,"22",("asd", 1), 122))
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[(Int, String, (String, Int), Int)]]

      result should be(Array((1,"test",("test", 1), 2), (1,"22",("asd", 1), 122)))
    }
  }

  "Default pickling" should {
    "unpickle a json pickled list" in {
      import scala.pickling._
      import scala.pickling.json._

      val pickle = List(12,13).pickle
      val result = pickle.unpickle[List[Int]]

      result should be(List(12,13))
    }
  }

}
object TestData {
  case class BlobberTest(name: String, age: Int)
}