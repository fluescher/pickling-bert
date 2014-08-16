package bert.format

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import bert.Bert.{InvalidTag, TermTooShort}
import bert.format.io.{ArrayOutput, Input, Output}

trait FormatWriter[T] {
  def write(out: Output, t: T): Output
}
trait FormatReader[T] {
  def read(in: Input): T
}

sealed trait CheckedReader[T] extends FormatReader[T] {

  protected val tag: Byte
  protected val minimumLength: Int

  @inline protected def readUnchecked(in: Input): T

  override def read(in: Input): T = {
    if (in.head != tag) { throw InvalidTag(in.head) }
    if(in.size < minimumLength) { throw TermTooShort() }

    readUnchecked(in)
  }
}

sealed abstract class TermFormat[T] extends FormatWriter[T] with CheckedReader[T]

object IntTermFormat extends TermFormat[Int] {
  override val tag: Byte = 98
  protected override val minimumLength: Int = 5
  
  override def write(out: Output, value: Int): Output = 
    out.put(tag.toByte)
       .put(ByteBuffer.allocate(4).putInt(value).array())

  protected override def readUnchecked(in: Input): Int =
    readValue(in)

  private def readValue(in: Input): Int = 
    ByteBuffer.wrap(in.consume(minimumLength).drop(1)).getInt()
    
}

object BinaryTermFormat extends TermFormat[Array[Byte]] {
  override val tag: Byte = 109
  protected override val minimumLength = 5

  override def write(out: Output, value: Array[Byte]): Output =
    out.put(tag)
       .put(ByteBuffer.allocate(4).putInt(value.length).array())
       .put(value)

  override def readUnchecked(in: Input): Array[Byte] = {
    val length = ByteBuffer.wrap(in.consume(5).drop(1)).getInt()
    in.consume(length)
  }
}

object SmallIntTermFormat extends TermFormat[Int] {
  override val tag: Byte = 97
  protected override val minimumLength = 2
  
  override def write(out: Output, value: Int): Output =
    out.put(tag)
       .put(ByteBuffer.allocate(4).putInt(value).array().drop(3))
       
  override def readUnchecked(in: Input): Int = {
    val buf = ByteBuffer.allocate(4)
              .put(Array(0.toByte,0.toByte,0.toByte))
              .put(in.consume(2).drop(1))
    buf.flip()
    buf.getInt()
  }
}

object DoubleTermFormat extends TermFormat[Double] {
  override val tag: Byte = 70
  protected override val minimumLength = 9
  
  override def write(out: Output, value: Double): Output = 
    out.put(tag)
       .put(ByteBuffer.allocate(8).putDouble(value).array())

  override def readUnchecked(in: Input): Double =
    ByteBuffer.wrap(in.consume(9).drop(1)).getDouble()
}

object StringTermFormat extends TermFormat[String] with CheckedReader[String] {
  override val tag: Byte = 107
  protected override val minimumLength = 3
  
  override def write(out: Output, value: String): Output =
    out.put(tag)
       .put(ByteBuffer.allocate(2).putShort(value.length().toShort).array())
       .put(value.getBytes(StandardCharsets.US_ASCII))

  override def readUnchecked(in: Input): String =
    readWithChecks(in)

  private def readWithChecks(in: Input) = {
    if(3 > in.size) throw TermTooShort()
    val stringLength = ByteBuffer.wrap(in.consume(3).drop(1)).getShort()
    readValue(in, stringLength)
  }

  private def readValue(in: Input, stringLength: Int): String =
    if(stringLength > in.size) throw TermTooShort()
    else new String(in.consume(stringLength), StandardCharsets.US_ASCII)

}

object NilTermFormat extends TermFormat[Nil.type] {
  override val tag: Byte = 106
  protected override val minimumLength = 1

  override def write(out: Output, value: Nil.type): Output = out.put(tag)
  override def readUnchecked(in: Input): Nil.type =  {
    in.consume(1)
    Nil
  }
}

object AtomTermFormat extends TermFormat[Symbol] {
  override val tag = 100.toByte
  protected override val minimumLength = 3

  override def readUnchecked(in: Input): Symbol = {
    val stringLength = ByteBuffer.wrap(in.consume(3).drop(1)).getShort()
    Symbol(new String(in.consume(stringLength), StandardCharsets.US_ASCII))
  }

  override def write(out: Output, t: Symbol): Output =
    out.put(tag)
      .put(ByteBuffer.allocate(2).putShort(t.name.length().toShort).array())
      .put(t.name.getBytes(StandardCharsets.US_ASCII))
}

/* Terrible hack to allow dynamic sized tuples */
object LargeTupleTermFormat {
  val tag = 105.toByte

  def writeHeader(output: Output, length: Int): Output =
    output.put(LargeTupleTermFormat.tag)
      .put(ByteBuffer.allocate(4).putInt(length).array())

  def writeElement[T](out: Output, element: T, format: TermFormat[T]): Unit =
    format.write(out, element)

  def readLength(in: Input): Int =
    ByteBuffer.wrap(in.consume(5).drop(1)).getInt()
}

trait SmallTupleTermFormat[T] extends TermFormat[T] {
  override val tag = SmallTupleTermFormat.tag
  protected override val minimumLength = SmallTupleTermFormat.minimumLength
}
object SmallTupleTermFormat {
  val tag = 104.toByte
  val minimumLength = 5

  def writeHeader(out: Output, arity: Int) = {
    out.put(tag)
    out.put(arity.toByte)
  }

}
class Tuple2TermFormat[T1,T2](private val t1Format: TermFormat[T1],
                              private val t2Format: TermFormat[T2]) extends SmallTupleTermFormat[(T1,T2)] {

  override def readUnchecked(in: Input): (T1, T2) = {
    in.consume(2)
    (t1Format.read(in), t2Format.read(in))
  }

  override def write(out: Output, t: (T1, T2)): Output = SmallTupleTermFormat.writeHeader(out, t.productArity)
}


class ListTermFormat[T](private val elementFormat: TermFormat[T]) extends TermFormat[List[T]] {
  override val tag = ListTermFormat.tag
  protected override val minimumLength = 5

  override def write(out: Output, value: List[T]): Output = {
    ListTermFormat.writeHeader(out, value.length)
    value.foreach(elementFormat.write(out, _))
    NilTermFormat.write(out, Nil)
    out
  }

  override def readUnchecked(in: Input): List[T] = {
    val elements = ListTermFormat.readLength(in)
    val readList = (0 until elements).map(_ => elementFormat.read(in)).toList
    NilTermFormat.read(in)
    readList
  }
}
object ListTermFormat {

  val tag = 108.toByte

  def readLength(in: bert.format.io.Input): Int =
    ByteBuffer.wrap(in.consume(5).drop(1)).getInt()

  def writeHeader(out: bert.format.io.Output, length: Int): Output = {
    out.put(tag)
       .put(ByteBuffer.allocate(4).putInt(length).array())
  }
}

class MapTermFormat[K,V](private val keyFormat: TermFormat[K],
                         private val valueFormat: TermFormat[V]) extends TermFormat[Map[K,V]] {
  override val tag: Byte = MapTermFormat.tag
  protected override val minimumLength = 5

  override def write(out: Output, value: Map[K,V]): Output = {
    out.put(MapTermFormat.tag)
       .put(ByteBuffer.allocate(4).putInt(value.size).array())
    value.foreach({case (key, value) =>
      keyFormat.write(out, key)
      valueFormat.write(out, value)
    })
    out
  }
  override def readUnchecked(in: Input): Map[K,V] = {
    val elements = ByteBuffer.wrap(in.consume(5).drop(1)).getInt()
    ((0 until elements).foldLeft(Map[K,V]())((map, _) => map + (keyFormat.read(in) -> valueFormat.read(in))))
  }
}
object MapTermFormat {
  val tag: Byte = 116
}