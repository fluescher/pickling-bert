package bert.format

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import bert.Bert.{InvalidTag, TermTooShort}
import bert.format.io.{Input, Output}

trait FormatWriter[T] {
  def write(out: Output, t: T): Output
}
trait FormatReader[T] {
  def read(in: Input): T
}

sealed trait CheckedReader[T] extends FormatReader[T] {

  protected val tag: Byte
  protected val minimumLength: Int

  @inline protected def readChecked(in: Input): T

  override def read(in: Input): T = {
    if (in.head != tag) { throw InvalidTag(in.head) }
    if(in.size < minimumLength) { throw TermTooShort() }

    readChecked(in)
  }
}

sealed abstract class TermFormat[T] extends FormatWriter[T] with CheckedReader[T]

object IntTermFormat extends TermFormat[Int] {
  override val tag: Byte = 98
  protected override val minimumLength: Int = 5
  
  override def write(out: Output, value: Int): Output = 
    out.put(tag.toByte)
       .put(ByteBuffer.allocate(4).putInt(value).array())

  protected override def readChecked(in: Input): Int =
    readValue(in)

  private def readValue(in: Input): Int = 
    ByteBuffer.wrap(in.consume(minimumLength).drop(1)).getInt()
    
}

object SmallIntTermFormat extends TermFormat[Int] {
  override val tag: Byte = 97
  protected override val minimumLength = 2
  
  override def write(out: Output, value: Int): Output = 
    out.put(tag)
       .put(ByteBuffer.allocate(4).putInt(value).array().drop(3))
       
  override def readChecked(in: Input): Int = {
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

  override def readChecked(in: Input): Double =
    ByteBuffer.wrap(in.consume(9).drop(1)).getDouble()
}

object StringTermFormat extends TermFormat[String] with CheckedReader[String] {
  override val tag: Byte = 107
  protected override val minimumLength = 3
  
  override def write(out: Output, value: String): Output =
    out.put(tag)
       .put(ByteBuffer.allocate(2).putShort(value.length().toShort).array())
       .put(value.getBytes(StandardCharsets.US_ASCII))

  override def readChecked(in: Input): String =
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
  override def readChecked(in: Input): Nil.type =  {
    in.consume(1)
    Nil
  }
}

object AtomTermFormat extends TermFormat[Symbol] {
  override val tag = 100.toByte
  protected override val minimumLength = 3

  override def readChecked(in: Input): Symbol = {
    val stringLength = ByteBuffer.wrap(in.consume(3).drop(1)).getShort()
    Symbol(new String(in.consume(stringLength), StandardCharsets.US_ASCII))
  }

  override def write(out: Output, t: Symbol): Output =
    out.put(tag)
      .put(ByteBuffer.allocate(2).putShort(t.name.length().toShort).array())
      .put(t.name.getBytes(StandardCharsets.US_ASCII))
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

  override def readChecked(in: Input): List[T] = {
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
  override def readChecked(in: Input): Map[K,V] = {
    val elements = ByteBuffer.wrap(in.consume(5).drop(1)).getInt()
    ((0 until elements).foldLeft(Map[K,V]())((map, _) => map + (keyFormat.read(in) -> valueFormat.read(in))))
  }
}
object MapTermFormat {
  val tag: Byte = 116
}