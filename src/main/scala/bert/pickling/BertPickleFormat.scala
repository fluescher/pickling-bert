package bert

import bert.format.io.ArrayInput
import bert.format.{DoubleTermFormat, IntTermFormat, ListTermFormat, NilTermFormat, StringTermFormat}

import scala.language.implicitConversions
import scala.pickling._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.Mirror

package object pickling {
  implicit val pickleFormat = new BertPickleFormat
  implicit def toBertPickle(value: Array[Byte]): BertPickle = BertPickle(value)
}

package pickling {

import bert.format.{SmallIntTermFormat, AtomTermFormat}

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

  final class BertPickleBuilder(format: BertPickleFormat) extends PBuilder with PickleTools {
    import format._
    
    private val output: bert.format.io.ArrayOutput = new bert.format.io.ArrayOutput()

    @inline override def beginEntry(picklee: Any): PBuilder = withHints { hints =>
      if(picklee == null) {
        NilTermFormat.write(output, Nil)
      } else {
        hints.tag.key match {
          case KEY_NULL => NilTermFormat.write(output, Nil)
          case KEY_BYTE => IntTermFormat.write(output, picklee.asInstanceOf[Byte])
          case KEY_SHORT => IntTermFormat.write(output, picklee.asInstanceOf[Short])
          case KEY_CHAR => IntTermFormat.write(output, picklee.asInstanceOf[Char])
          case KEY_INT => IntTermFormat.write(output, picklee.asInstanceOf[Int])
          case KEY_LONG => IntTermFormat.write(output, picklee.asInstanceOf[Long].toInt)
          case KEY_BOOLEAN => picklee.asInstanceOf[Boolean] match { // TODO this should be mapped to true and false atoms
            case true => SmallIntTermFormat.write(output, 1)
            case false => SmallIntTermFormat.write(output, 0)
          }
          case KEY_FLOAT => DoubleTermFormat.write(output, picklee.asInstanceOf[Float])
          case KEY_DOUBLE => DoubleTermFormat.write(output, picklee.asInstanceOf[Double])
          case KEY_SCALA_STRING | KEY_JAVA_STRING => StringTermFormat.write(output, picklee.asInstanceOf[String])
          case KEY_ARRAY_BYTE =>
          case KEY_ARRAY_CHAR =>
          case KEY_ARRAY_SHORT =>
          case KEY_ARRAY_INT => writeArray(picklee.asInstanceOf[Array[Int]], (i: Int) => IntTermFormat.write(output, i))
          case KEY_ARRAY_LONG =>
          case KEY_ARRAY_BOOLEAN =>
          case KEY_ARRAY_FLOAT =>
          case KEY_ARRAY_DOUBLE =>
          case _ @ s if !s.startsWith("scala.collection.immutable.$colon$colon") => /* special case if list is used */
            AtomTermFormat.write(output, 'Object)
            StringTermFormat.write(output, hints.tag.key)
          case _ =>
        }
      }
      this
    }

    @inline override def putField(name: String, pickler: PBuilder => Unit): PBuilder = {
      pickler(this)
      this
    }

    @inline override def endEntry(): Unit = { /* do nothing */ }

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

    private def writeArray[T](arr: Array[T], pickler: T => Unit) {
      ListTermFormat.writeHeader(output, arr.length)
      var i = 0
      while(i < arr.length) {
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
      withHints { hints => input.head match {
          case NilTermFormat.tag => FastTypeTag.Null.key
          case ListTermFormat.tag => "scala.collection.immutable.$colon$colon[scala.Int]"
          case AtomTermFormat.tag => {
            AtomTermFormat.read(input) match {
              case 'Object => StringTermFormat.read(input)
            }
          }
          case _ =>  hints.tag.key
        }
      }
    }

    override def beginEntry(): FastTypeTag[_] = ???

    override def atPrimitive: Boolean = input.head match {
      case AtomTermFormat.tag => false
      case _ => true
    }

    override def readPrimitive(): Any = withHints { hints => hints.tag.key match {
      case KEY_BYTE =>  IntTermFormat.read(input).toByte
      case KEY_SHORT => IntTermFormat.read(input).toShort
      case KEY_CHAR => IntTermFormat.read(input).toChar
      case KEY_INT => IntTermFormat.read(input)
      case KEY_LONG => IntTermFormat.read(input).toLong
      case KEY_BOOLEAN => SmallIntTermFormat.read(input) match {
        case 0 => false
        case _ => true
      }
      case KEY_FLOAT => DoubleTermFormat.read(input).toFloat
      case KEY_DOUBLE => DoubleTermFormat.read(input)
      case KEY_SCALA_STRING | KEY_JAVA_STRING => StringTermFormat.read(input)
      case KEY_NULL => NilTermFormat.read(input)
      case _ => input.head match {
        case NilTermFormat.tag => NilTermFormat.read(input); null
        case _ => readArray(readLength(), IntTermFormat.read(input))
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
