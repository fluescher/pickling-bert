package bert

import scala.pickling._
import scala.language.implicitConversions
import scala.pickling.internal._
import scala.reflect.runtime.universe.Mirror
import bert.format.IntTermFormat
import bert.format.NilTermFormat
import bert.format.ListTermFormat
import bert.format.io.ArrayInput
import bert.format.StringTermFormat
import bert.format.DoubleTermFormat

package object pickling {
  implicit val pickleFormat = new BertPickleFormat
  implicit def toBertPickle(value: Array[Byte]): BertPickle = BertPickle(value)
}

package pickling {
  import bert.pickling._

  abstract class BertPickle extends Pickle {
    type PickleFormatType = BertPickleFormat
    type ValueType = Array[Byte]

    override val value: Array[Byte]

    def createReader(mirror: Mirror, format: BertPickleFormat): PReader
  }

  case class BertPickleArray(data: Array[Byte]) extends BertPickle {
    override val value = data
    
    override def createReader(mirror: Mirror, format: BertPickleFormat): PReader =
      /* consume term format version. TODO should be checked */
      new BertPickleReader(new ArrayInput(data.drop(1)), mirror, format)

    override def toString: String = s"""BertPickle(${value.mkString("[", ",", "]")})"""
  }

  object BertPickle {
    def apply(a: Array[Byte]): BertPickle =
      new BertPickleArray(a)
  }

  final class BertPickleBuilder(format: BertPickleFormat) extends PBuilder with PickleTools {
    import format._
    
    private var output: bert.format.io.ArrayOutput = new bert.format.io.ArrayOutput()

    @inline override def beginEntry(picklee: Any): PBuilder = withHints { hints =>
      hints.tag.key match {
          case KEY_NULL => NilTermFormat.write(output, Nil)
          case KEY_BYTE =>
          case KEY_SHORT =>
          case KEY_CHAR => 
          case KEY_INT => IntTermFormat.write(output, picklee.asInstanceOf[Int])
          case KEY_LONG => 
          case KEY_BOOLEAN =>
          case KEY_FLOAT => 
          case KEY_DOUBLE => DoubleTermFormat.write(output, picklee.asInstanceOf[Double])
          case KEY_SCALA_STRING | KEY_JAVA_STRING => StringTermFormat.write(output, picklee.asInstanceOf[String])
          case KEY_ARRAY_BYTE =>
          case KEY_ARRAY_CHAR =>
          case KEY_ARRAY_SHORT =>
          case KEY_ARRAY_INT =>
          case KEY_ARRAY_LONG =>
          case KEY_ARRAY_BOOLEAN =>
          case KEY_ARRAY_FLOAT =>
          case KEY_ARRAY_DOUBLE =>
          case _ =>
      }
      this
    }

    @inline override def putField(name: String, pickler: PBuilder => Unit): PBuilder = {
      this
    }

    @inline override def endEntry(): Unit = { /* do nothing */ }

    @inline override def beginCollection(length: Int): PBuilder = {

      println(s"length: $length")
      ListTermFormat.writeHeader(output, length)
      this
    }

    @inline override def putElement(pickler: PBuilder => Unit): PBuilder = {
      pickler(this)
      this
    }

    @inline override def endCollection(): Unit = 
      NilTermFormat.write(output, Nil)

    @inline override def result() = 
      BertPickle(Array(Bert.PROTOCOL_VERSION) ++ output.toArray)
  }
  
  class BertPickleReader(input: ArrayInput, val mirror: Mirror, format: BertPickleFormat) extends PReader with PickleTools {
    import format._

    override def beginEntryNoTag(): String = beginEntry().key

    def beginEntry(): FastTypeTag[_] = {println(input.head); input.head match {
      case IntTermFormat.tag     => FastTypeTag.Int
      case StringTermFormat.tag  => FastTypeTag.ScalaString
      case DoubleTermFormat.tag  => FastTypeTag.Double
      case NilTermFormat.tag     => FastTypeTag.Null
      case ListTermFormat.tag    => FastTypeTag(mirror, "scala.collection.immutable.$colon$colon[scala.Int]")
    }}

    override def atPrimitive: Boolean = input.head match {
      case IntTermFormat.tag     => true
      case StringTermFormat.tag  => true
      case DoubleTermFormat.tag  => true
      case NilTermFormat.tag     => true
      case _                     => false
    }

    override def readPrimitive(): Any = input.head match {
      case IntTermFormat.tag     => IntTermFormat.read(input)
      case StringTermFormat.tag  => StringTermFormat.read(input) 
      case DoubleTermFormat.tag  => DoubleTermFormat.read(input)
      case NilTermFormat.tag     => NilTermFormat.read(input); null
    }

    override def atObject: Boolean = false

    override def readField(name: String): BertPickleReader = this

    override def endEntry(): Unit = { }

    override def beginCollection(): PReader = {println("ICI"); this}  

    override def readLength(): Int = {val l = ListTermFormat.readLength(input); println("LENGTH: " + l); l}

    override def readElement(): PReader = this

    override def endCollection(): Unit = NilTermFormat.read(input)
      
  }
  
  class BertPickleFormat extends PickleFormat {
    type PickleType = BertPickle
    type OutputType = Output[Array[Byte]]
    def createBuilder(): BertPickleBuilder = new BertPickleBuilder(this)

    override def createBuilder(out: Output[Array[Byte]]): BertPickleBuilder = createBuilder()

    override def createReader(pickle: PickleType, mirror: Mirror): PReader = pickle.createReader(mirror, this)

    val KEY_NULL = FastTypeTag.Null.key
    val KEY_BYTE = FastTypeTag.Byte.key
    val KEY_SHORT = FastTypeTag.Short.key
    val KEY_CHAR = FastTypeTag.Char.key
    val KEY_INT = FastTypeTag.Int.key
    val KEY_LONG = FastTypeTag.Long.key
    val KEY_BOOLEAN = FastTypeTag.Boolean.key
    val KEY_FLOAT = FastTypeTag.Float.key
    val KEY_DOUBLE = FastTypeTag.Double.key
    val KEY_UNIT = FastTypeTag.Unit.key

    val KEY_SCALA_STRING = FastTypeTag.ScalaString.key
    val KEY_JAVA_STRING = FastTypeTag.JavaString.key

    val KEY_ARRAY_BYTE = FastTypeTag.ArrayByte.key
    val KEY_ARRAY_SHORT = FastTypeTag.ArrayShort.key
    val KEY_ARRAY_CHAR = FastTypeTag.ArrayChar.key
    val KEY_ARRAY_INT = FastTypeTag.ArrayInt.key
    val KEY_ARRAY_LONG = FastTypeTag.ArrayLong.key
    val KEY_ARRAY_BOOLEAN = FastTypeTag.ArrayBoolean.key
    val KEY_ARRAY_FLOAT = FastTypeTag.ArrayFloat.key
    val KEY_ARRAY_DOUBLE = FastTypeTag.ArrayDouble.key
  }
}
