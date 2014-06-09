package bert.format.io

import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.Checkers
import org.scalatest.prop.PropertyChecks

class IOSpec extends WordSpec with Matchers with Checkers with PropertyChecks {
  "Input" should {
    "have a lookahead value" in {
      val in = new ArrayInput(Array(12.toByte, 13.toByte))
      
      in.head shouldBe(12)
      in.consume(1)
      in.head shouldBe(13)
    }
    
    "return the consumed data" in {
      val in = new ArrayInput(Array(12.toByte, 13.toByte))
      
      in.consume(1) should be(Array(12.toByte))
      in.consume(1) should be(Array(13.toByte))
    }
    
    "adjust its size" in {
      val in = new ArrayInput(Array(12.toByte, 13.toByte))
      
      in.size should be(2)
      in.consume(1)
      in.size should be(1)
      in.consume(1) 
      in.size should be(0)
    }
  }
  
  "Output" should {
    "add a single byte to the output" in {
      val out = new ArrayOutput()
      
      out.put(12.toByte)
      
      out.toArray should be(Array(12.toByte))
    }
    
    "add multiple single bytes to the output" in {
      val out = new ArrayOutput()
      
      out.put(12.toByte)
      out.put(13.toByte)
      
      out.toArray should be(Array(12.toByte, 13.toByte))
    }
    
    "add multiple bytes to the output" in {
      val out = new ArrayOutput()
      
      out.put(Array(12.toByte, 13.toByte))
      
      out.toArray should be(Array(12.toByte, 13.toByte))
    }
    
    "add multple bytes after a single put" in {
      val out = new ArrayOutput()
      
      out.put(11.toByte)
      out.put(Array(12.toByte, 13.toByte))
      
      out.toArray should be(Array(11.toByte, 12.toByte, 13.toByte))
    }
  }
}