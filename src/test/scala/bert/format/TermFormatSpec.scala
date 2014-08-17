package bert.format

import bert.Bert.TermTooShort
import bert.BertSpec._
import bert.format.io.ArrayInput._
import bert.format.io._
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalatest.{Matchers, WordSpec}

class TermFormatSpec extends WordSpec with Matchers with Checkers with PropertyChecks {
  "Int Term Format" should {
    "write the int value" in {
      val encoded = IntTermFormat.write(new ArrayOutput(), 12).toArray 

      encoded.length should be(5)
      encoded(1) should be(0.toByte)
      encoded(2) should be(0.toByte)
      encoded(3) should be(0.toByte)
      encoded(4) should be(12.toByte)
    }
    
    "write the int type tag" in {
      val encoded = IntTermFormat.write(new ArrayOutput(), 12).toArray 

      encoded.length should be(5)
      encoded(0) should be(98.toByte)
    }
    
    "decode an Int" in {
      val in = Array(98,0,0,4,176).map(_.toByte)
      
      val res = IntTermFormat.read(in)
      
      res should be(1200)
    }
    
    "throw an exception if the input is too short" in {
      intercept[TermTooShort] {
        val in = Array(98,0,0,4).map(_.toByte)
        
        IntTermFormat.read(in)
      }
    }
    
    "have the same value when decoded" in
      check((a: Int) => 
        IntTermFormat.read(IntTermFormat.write(new ArrayOutput(), a).toArray) == a)
  }
  
  "Float Term Format" should {
    "start with the type tag" in {
      val encoded = DoubleTermFormat.write(new ArrayOutput(), 12.0).toArray
        
      encoded(0) should be(70)
    }
    
    "be able to decode a Float" in {
      val encoded = Array(70,64,40,153,153,153,153,153,154).map(_.toByte)

      val decoded  = DoubleTermFormat.read(encoded)
      
      decoded should be(12.3)
    }
  }
  
  "Small Integer Term Format" should {
    "start with the type tag" in {
      val encoded = SmallIntTermFormat.write(new ArrayOutput(), 12).toArray
      
      encoded(0) should be(97)
    }
    "should be able to decode a number" in {
      val encoded = Array(97,13).map(_.toByte)
      val decoded = SmallIntTermFormat.read(encoded)
      
      decoded should be(13)
    }
  }
  
  "String Term Format" should {
    "start with the type tag" in {
      val encoded = StringTermFormat.write(new ArrayOutput(), "test").toArray
        
      encoded(0) should be(107)
    }

    "encode the emtpty string as nill" in {
      val encoded = StringTermFormat.write(new ArrayOutput(), "").toArray

      encoded(0) should be(106)
    }

    "be able to encode the empty string" in {
      val encoded = StringTermFormat.write(new ArrayOutput(), "").toArray
      val decoded = StringTermFormat.read(encoded)
      
      decoded should be("")
    }
    
    "be able to decode a simple string" in {
      val encoded = Array(107,0,12,72,101,108,108,111,32,115,99,97,108,97,33).map(_.toByte)
      val decoded = StringTermFormat.read(encoded)

      decoded should be("Hello scala!")
    }
    
    "throw an exception if the input is too short" in {
      intercept[TermTooShort] {
        val encoded = Array(107,0,12,72,101,108,108,111,32,115,99,97,108,97).map(_.toByte)
        StringTermFormat.read(encoded)
      }
    }
    
    "have the same value when decoded" in
      forAll (asciiStrings){(a: String) => {
        StringTermFormat.read(StringTermFormat.write(new ArrayOutput(),a).toArray) should be(a)
      }}
  }
  
  "Map Term Format" should {
    val stringToIntMapFormat = new MapTermFormat(StringTermFormat, IntTermFormat)
    
    "start with the type tag" in {
      val encoded = stringToIntMapFormat.write(new ArrayOutput(), Map()).toArray
      
      encoded(0) should be(116)
    }
    
    "be able to decode a Map" in {
      val encoded = Array(116,0,0,0,1,107,0,2,75,49,98,0,0,4,176).map(_.toByte)
      
      val decoded = stringToIntMapFormat.read(encoded)
      
      decoded should be(Map("K1" -> 1200))
    }
    
    "be able to encode a Map" in {
      val encoded = stringToIntMapFormat.write(new ArrayOutput(), Map("K1" -> 1200)).toArray 
      
      encoded should be(Array(116,0,0,0,1,107,0,2,75,49,98,0,0,4,176).map(_.toByte))
    }
  }

  "Tuple Term Format" should {
    "start with the type tag" in {
      val encoded = new Tuple2TermFormat(IntTermFormat, IntTermFormat).write(new ArrayOutput(),(1,2)).toArray

      encoded(0) should be(104)
    }
    "contain the arity in the header" in {
      val encoded = new Tuple2TermFormat(IntTermFormat, IntTermFormat).write(new ArrayOutput(),(1,2)).toArray

      encoded(1) should be(2)
    }
    "be able to decode an Erlang tuple" in {
      val encoded = Array(104,2,97,1,97,2).map(_.toByte)
      val decoded = new Tuple2TermFormat(SmallIntTermFormat, SmallIntTermFormat).read(encoded)

      decoded should be((1, 2))
    }
  }

  "Atom Term Format" should {
    "start with the type tag" in {
      val encoded = AtomTermFormat.write(new ArrayOutput(), 'Atom).toArray

      encoded.length should be(7)
      encoded(0) should be(100)
    }
    "store the correct length information" in {
      val encoded = AtomTermFormat.write(new ArrayOutput(), 'Atom).toArray

      encoded(1) should be(0)
      encoded(2) should be(4)
    }
    "be able to decode an Erlang Atom" in {
      val encoded = Array(100,0,5,104,101,108,108,111).map(_.toByte)
      val decoded = AtomTermFormat.read(encoded)

      decoded should equal('hello)
    }
  }

  "Binary Term Format" should {
    "start with the type tag" in {
      val encoded = BinaryTermFormat.write(new ArrayOutput(), Array(12.toByte)).toArray

      encoded.length should be(6)
      encoded(0) should be(109)
    }
    "store the correct length information" in {
      val encoded = BinaryTermFormat.write(new ArrayOutput(), Array(12.toByte,-12.toByte)).toArray

      encoded(1) should be(0)
      encoded(2) should be(0)
      encoded(3) should be(0)
      encoded(4) should be(2)
    }
    "store the correct value" in {
      val encoded = BinaryTermFormat.write(new ArrayOutput(), Array(12.toByte)).toArray

      encoded(5) should be(12)
    }
    /* term_to_binary(term_to_binary("Hello scala")). */
    "be able to decode a erlang byte term" in {
      val encoded = Array(109,0,0,0,15,131,107,0,11,72,101,108,108,111,32,115,
                          99,97,108,97).map(_.toByte)
      val decoded = BinaryTermFormat.read(encoded)

      decoded should be(Array(131,107,0,11,72,101,108,108,111,32,115,99,97,108,97).map(_.toByte))
    }
  }

  "List Term Format" should {
    val intListFormat = new ListTermFormat(IntTermFormat)
    val stringListFormat = new ListTermFormat(StringTermFormat)
    
    "start with the type tag" in {
      val encoded = intListFormat.write(new ArrayOutput(), List(12)).toArray

      encoded.length should be(11)
      encoded(9) should be(12)
    }
    
    "end with Nil" in {
      val encoded = intListFormat.write(new ArrayOutput(), List(1)).toArray
      
      encoded(10) should be(106)
    }

    "be able to hold one element" in {
      val encoded = intListFormat.write(new ArrayOutput(), List(11)).toArray
      val decoded = intListFormat.read(encoded)

      decoded.length should be(1)
    }

    "be able to hold two elements" in {
      val encoded = intListFormat.write(new ArrayOutput(), List(11, 12)).toArray
      val decoded = intListFormat.read(encoded)

      decoded.length should be(2)
      decoded should be(List(11, 12))
    }
    
    "be able to hold Strings" in {
      val encoded = stringListFormat.write(new ArrayOutput(), List("test", "Hello scala!")).toArray
      val decoded = stringListFormat.read(encoded)
      
      decoded should be(List("test", "Hello scala!"))
    }
    
    "be able to read an Erlang Int list" in {
      val encoded = Array(108,0,0,0,3,98,0,0,4,176,98,0,0,5,20,98,0,0,5,120,106).map(_.toByte)
      val decoded = intListFormat.read(encoded)
      
      decoded should be(List(1200, 1300, 1400))
    }
    
    "be able to read an Erlang String list" in {
      val encoded = Array(108,0,0,0,7,107,0,5,72,101,108,108,111,107,0,1,32,107,0,6,83,99,97,108,
                          97,33,107,0,1,32,107,0,9,71,114,101,101,116,105,110,103,115,107,0,1,32,
                          107,0,6,69,114,108,97,110,103,106).map(_.toByte)
      val decoded = stringListFormat.read(encoded)
      
      decoded should be(List("Hello", " ", "Scala!", " ", "Greetings", " ", "Erlang"))
    }

    "have the same int value when decoded" in
      check((a: List[Int]) => 
        intListFormat.read(intListFormat.write(new ArrayOutput,a).toArray) == a)
    
    "have the same complex value when decoded" in {
      val complexFormat = new ListTermFormat(stringListFormat)
      
      forAll (listOfListOfAsciiStrings, maxSize(20)){(a: List[List[String]]) => 
        complexFormat.read(complexFormat.write(new ArrayOutput, a).toArray) should be(a)
      }
    }
      
  }
}