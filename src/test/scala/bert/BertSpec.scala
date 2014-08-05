package bert

import bert.Bert.{InvalidTag, TermTooShort}
import org.scalacheck.Gen
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalatest.{Matchers, WordSpec}

import scala.util.Failure

class BertSpec extends WordSpec with Matchers with Checkers with PropertyChecks {

  import bert.Bert.PROTOCOL_VERSION
  import bert.BertSpec._
  
  "A Bert encoded Int" should {
    "start with the distribution header" in {
      val encoded = Bert.toBert(12)

      encoded(0) should be(PROTOCOL_VERSION.toByte)
    }

    "fail if input has wrong tag" in {
      val decoded = Bert.fromBert[Int](Array(PROTOCOL_VERSION, 12.toByte,
        0.toByte, 0.toByte,
        0.toByte, 0.toByte))

      decoded shouldBe 'failure
      decoded should be(Failure(InvalidTag(12)))
    }

    "fail if input is too short" in {
      val decoded = Bert.fromBert[Int](Array(PROTOCOL_VERSION, 98.toByte,
        0.toByte, 0.toByte,
        0.toByte))

      decoded shouldBe 'failure
      decoded should be(Failure(TermTooShort()))
    }

    "have the same value when decoded" in
      check((a: Int) => Bert.fromBert[Int](Bert.toBert(a)).get == a)
  }

  "A Bert encoded list" should {
    "have the same value when decoded" in
      check((a: List[Int]) => Bert.fromBert[List[Int]](Bert.toBert(a)).get == a)
  }
  
  "A Bert encoded String" should {
    "have the same value when decoded" in
      forAll (asciiStrings){(a: String) => {
        Bert.fromBert[String](Bert.toBert(a)).get should be(a)
      }}
  }

  "A bert encoded Float" should {
    "have the same value when decoded" in
      check((a: Double) => Bert.fromBert[Double](Bert.toBert(a)).get == a)
  }
  
  "A bert encoded Map" should {
    "have the same value when decoded" in {
      forAll (mapOfStringToListOfStrings){(a: Map[String,List[String]]) => {
        Bert.fromBert[Map[String,List[String]]](Bert.toBert(a)).get should be(a)
      }}
    }
  }
  
  "A term written to a file" should {
    "be readable by erlang" in {
      val encoded = Bert.toBert(List(11,12))
      toFile(encoded)
      
      def toFile(term: Array[Byte], fileName: String = "out.bert") = {
        val out = new java.io.FileOutputStream(fileName)
        out.write(term)
        out.close
      }
    }
  }
  
}
object BertSpec {
  def mapOfStringToListOfStrings: Gen[Map[String,List[String]]] = for {
     k <- asciiStrings
     v <- Gen.listOf(asciiStrings)
     m <- Gen.oneOf[Option[Map[String,List[String]]]](None, mapOfStringToListOfStrings.map(g => Some(g)))
  } yield (for (x <- m) yield x.updated(k, v)).getOrElse(Map())
  
  def listOfListOfAsciiStrings = Gen.listOf(Gen.listOf(asciiStrings))
  
  def asciiScalar: Gen[Int] = Gen.choose(0, 127)
  def asciiChars: Gen[Array[Char]] = asciiScalar map (cp => Character.toChars(cp))
  def asciiStrings: Gen[String] = for (css <- Gen.listOf(asciiChars)) yield {
    css.flatten.mkString
  }
}
