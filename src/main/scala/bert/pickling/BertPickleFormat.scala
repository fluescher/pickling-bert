package bert

import bert.format.io.ArrayInput
import bert.format.{DoubleTermFormat, IntTermFormat, ListTermFormat, NilTermFormat, StringTermFormat, LargeTupleTermFormat}

import scala.language.implicitConversions
import scala.pickling._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.Mirror

package object pickling {
  implicit val pickleFormat = new BertPickleFormat

  implicit def toBertPickle(value: Array[Byte]): BertPickle = BertPickle(value)
}

package pickling {

import bert.format._

abstract class BertPickle extends Pickle {
  type PickleFormatType = BertPickleFormat
  type ValueType = Array[Byte]

  override val value: Array[Byte]

  def createReader(mirror: Mirror, format: BertPickleFormat): PReader
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


final class BertPickleBuilder(format: BertPickleFormat, private var parentBuilder: BertPickleBuilder = null) extends PBuilder with PickleTools {

  import format._

  private var stack: List[(Int, Int, bert.format.io.ArrayOutput)] = Nil
  private var fieldCount = 0
  private var ident = 0
  private var output: bert.format.io.ArrayOutput = new bert.format.io.ArrayOutput()

  @inline override def beginEntry(picklee: Any): PBuilder = withHints { hints =>
    ident = ident + 1
    if (picklee == null) {
      NilTermFormat.write(output, Nil); this
    } else {
      hints.tag.key match {
        case KEY_NULL => NilTermFormat.write(output, Nil); this
        case KEY_BYTE => IntTermFormat.write(output, picklee.asInstanceOf[Byte]); this
        case KEY_SHORT => IntTermFormat.write(output, picklee.asInstanceOf[Short]); this
        case KEY_CHAR => IntTermFormat.write(output, picklee.asInstanceOf[Char]); this
        case KEY_INT => IntTermFormat.write(output, picklee.asInstanceOf[Int]); this
        case KEY_LONG => IntTermFormat.write(output, picklee.asInstanceOf[Long].toInt); this
        case KEY_BOOLEAN => picklee.asInstanceOf[Boolean] match {
          case true => AtomTermFormat.write(output, 'true)
          case false => AtomTermFormat.write(output, 'false)
        }; this
        case KEY_FLOAT => DoubleTermFormat.write(output, picklee.asInstanceOf[Float]); this
        case KEY_DOUBLE => DoubleTermFormat.write(output, picklee.asInstanceOf[Double]); this
        case KEY_SCALA_STRING | KEY_JAVA_STRING => StringTermFormat.write(output, picklee.asInstanceOf[String]); this
        case KEY_ARRAY_BYTE => this
        case KEY_ARRAY_CHAR => this
        case KEY_ARRAY_SHORT => this
        case KEY_ARRAY_INT => writeArray(picklee.asInstanceOf[Array[Int]], (i: Int) => IntTermFormat.write(output, i)); this
        case KEY_ARRAY_LONG =>this
        case KEY_ARRAY_BOOLEAN => this
        case KEY_ARRAY_FLOAT => this
        case KEY_ARRAY_DOUBLE => this
        case _@classTag => /* write complex object */
          saveState()
          AtomTermFormat.write(output, Symbol(classTag))
          this
      }
    }
  }

  @inline override def putField(name: String, pickler: PBuilder => Unit): PBuilder = {
    fieldCount = fieldCount + 1
    pickler(this)
    this
  }

  @inline override def endEntry(): Unit = {
    ident = ident - 1
    if(ident == 0 || (stack != Nil && stack.head._2 == ident)) {
      restoreState()
    }
  }

  @inline override def beginCollection(length: Int): PBuilder = {
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


  private def saveState() = {
    stack = (fieldCount, ident, output) :: stack
    fieldCount = 0
    output = new bert.format.io.ArrayOutput()
  }

  private def restoreState() = {
    if(stack != Nil) {
      val (cnt, _, out) = stack.head

      LargeTupleTermFormat.writeHeader(out, fieldCount)
      out.put(output.toArray)

      stack = stack.tail
      output = out
      fieldCount = cnt
    }
  }

  private def writeArray[T](arr: Array[T], pickler: T => Unit) {
    ListTermFormat.writeHeader(output, arr.length)
    var i = 0
    while (i < arr.length) {
      pickler(arr(i))
      i += 1
    }
    NilTermFormat.write(output, Nil)
  }
}


class BertPickleReader(input: ArrayInput, val mirror: Mirror, format: BertPickleFormat) extends PReader with PickleTools {

  import format._

  override def beginEntryNoTag(): String = {
    pinHints() // TODO Check if this is really necessary and if it causes any problems
    withHints { hints =>
      if(atObject) {
        LargeTupleTermFormat.readLength(input)
        AtomTermFormat.read(input).name
      } else {
        hints.tag.key
      }
    }
  }

  override def beginEntry(): FastTypeTag[_] = ???

  override def atPrimitive: Boolean = withHints { hints => input.head match {
    case SmallTupleTermFormat.tag => false
    case LargeTupleTermFormat.tag => false
    case _ => true
  }}

  override def readPrimitive(): Any = withHints { hints => hints.tag.key match {
    case KEY_BYTE => IntTermFormat.read(input).toByte
    case KEY_SHORT => IntTermFormat.read(input).toShort
    case KEY_CHAR => IntTermFormat.read(input).toChar
    case KEY_INT => IntTermFormat.read(input)
    case KEY_LONG => IntTermFormat.read(input).toLong
    case KEY_BOOLEAN => AtomTermFormat.read(input) match {
      case 'true => true
      case 'false => false
    }
    case KEY_FLOAT => DoubleTermFormat.read(input).toFloat
    case KEY_DOUBLE => DoubleTermFormat.read(input)
    case KEY_SCALA_STRING | KEY_JAVA_STRING => StringTermFormat.read(input)
    case KEY_NULL => NilTermFormat.read(input)
    case _ => input.head match {
      case NilTermFormat.tag => NilTermFormat.read(input); null
      case _ =>
        readArray(readLength(), IntTermFormat.read(input))
    }
  }}

  override def atObject: Boolean = !atPrimitive

  override def readField(name: String): BertPickleReader = this

  override def endEntry(): Unit = {}

  override def beginCollection(): PReader = this

  override def readLength(): Int = ListTermFormat.readLength(input)

  override def readElement(): PReader = this

  override def endCollection(): Unit = NilTermFormat.read(input)

  private def readArray[T: ClassTag](length: Int, unpickler: => T): Array[T] = {
    var i = 0
    val arr = Array.ofDim[T](length)
    while (i < length) {
      arr(i) = unpickler
      i += 1
    }
    NilTermFormat.read(input)
    arr
  }

}

}
