package bert

import com.ericsson.otp.erlang.OtpOutputStream
import org.scalatest.prop.{PropertyChecks, Checkers}
import org.scalatest.{Matchers, WordSpec}

/** Verifies the correctness of the written term by cross checking them with the Erlang JInterface implementation */
class InteropTest extends WordSpec with Matchers with Checkers with PropertyChecks {
  import BertSpec._

  "A Bert term" should {
    "be able to write a String" in { forAll (asciiStrings){ (a: String) => {
        val otpData = toErts(a)
        val bertData = Bert.toBertTerm(a)

        bertData should be(otpData)
    }}}
    "be able to read a String" in { forAll (asciiStrings){ (a: String) => {
      val otpData = toErts(a)
      val decoded = Bert.fromBertTerm[String](otpData)

      decoded.get should be(a)
    }}}
  }

  "A pickled Bert term" should {
    import scala.pickling._
    import bert.pickling._

    "be able to read a pickled value" in { forAll (asciiStrings){ (a: String) => {
      val otpData = toErts(a)
      val pickle = a.pickle.value.drop(1)

      pickle should be (otpData)
    }}}
  }

  private def toErts(string: String): Array[Byte] = {
    val otpOutput = new OtpOutputStream()
    otpOutput.write_string(string)
    otpOutput.toByteArray()
  }
}
