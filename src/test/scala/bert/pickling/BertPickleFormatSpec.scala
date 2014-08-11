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

    "unpickle a pickled list" in {
      val pickle = List(1, 2, 3, 4).pickle
      val result = pickle.unpickle[List[Int]]

      result should be(List(1, 2, 3, 4))
    }
    "produce a Bert compatible pickled list" in {
      val pickle = List(1, 2, 3, 4).pickle
      val result = Bert.fromBert[List[Int]](pickle.value).get

      result should be(List(1, 2, 3, 4))
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
    "unpickle an int array" in {
      val arr = Array(1, 2, 3)
      val pickle = arr.pickle
      val result = pickle.unpickle[Array[Int]]

      result should be(Array(1, 2, 3))
    }
    "unpicke a simple case class" in {
      val blobber = BlobberTest("Blobber", 13)
      val pickle = blobber.pickle
      val result = pickle.unpickle[BlobberTest]

      result should be(blobber)
    }
    "unpickle a binary tuple" in {
      val pickle = ("test",2).pickle
      val result = pickle.unpickle[(String,Int)]

      result should be(("test",2))
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