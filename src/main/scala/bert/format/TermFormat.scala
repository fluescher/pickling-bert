package bert.format

import java.util.Arrays
import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
import scala.pickling.binary.ByteBuffer
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import bert.format.io.Output
import bert.format.io.Input
import bert.format.io.ArrayInput._
import bert.Bert.TermTooShort
import bert.Bert.InvalidTag


trait FormatWriter[T] {
  def write(out: Output, t: T): Output
}
trait FormatReader[T] {
  def read(in: Input): T
}

sealed abstract class TermFormat[T] extends FormatWriter[T] with FormatReader[T] 

object TermFormat {
  def readWithTypeTag[T](tag: Byte, reader: Input => T)(in: Input): T =
    if (in.head != tag) { throw InvalidTag(in.head) }
    else {reader(in)}
  
  def readWithLengthCheck[T](length: Int, reader: Input => T)(in: Input): T = 
    if(in.size < length) { throw TermTooShort() }
    else {reader(in)}
}

object IntTermFormat extends TermFormat[Int] {
  import TermFormat._
  
  val tag: Byte = 98
  val length: Int = 5
  
  override def write(out: Output, value: Int): Output = 
    out.put(tag.toByte)
       .put(ByteBuffer.allocate(4).putInt(value).array())

  override def read(in: Input): Int = 
    readWithChecks(in)
    
  private val readWithChecks: Input => Int =
    readWithLengthCheck(length, readWithTypeTag(tag, readValue))
  
  private def readValue(in: Input): Int = 
    ByteBuffer.wrap(in.consume(5).drop(1)).getInt()
    
}

object SmallIntTermFormat extends TermFormat[Int] {
  val tag: Byte = 97
  
  override def write(out: Output, value: Int): Output = 
    out.put(tag)
       .put(ByteBuffer.allocate(4).putInt(value).array().drop(3))
       
  override def read(in: Input): Int = { 
    val buf = ByteBuffer.allocate(4)
              .put(Array(0.toByte,0.toByte,0.toByte))
              .put(in.consume(2).drop(1))
    buf.flip()
    buf.getInt()
  }
}

object DoubleTermFormat extends TermFormat[Double] {
   val tag: Byte = 70
  
  override def write(out: Output, value: Double): Output = 
    out.put(tag)
       .put(ByteBuffer.allocate(8).putDouble(value).array())

  override def read(in: Input): Double = 
    ByteBuffer.wrap(in.consume(9).drop(1)).getDouble()
}

object StringTermFormat extends TermFormat[String] {
  import TermFormat._
  
  val tag: Byte = 107
  val length: Int = 3
  
  override def write(out: Output, value: String): Output = {
    out.put(tag)
       .put(ByteBuffer.allocate(2).putShort(value.length().toShort).array())
       .put(value.getBytes(StandardCharsets.US_ASCII))
  }
  
  override def read(in: Input): String = 
    readWithChecks(in)    
  
  private val readWithChecks: Input => String =
    TermFormat.readWithLengthCheck(length, TermFormat.readWithTypeTag(tag, in => {
      val stringLength = ByteBuffer.wrap(in.consume(3).drop(1)).getShort()
      TermFormat.readWithLengthCheck(stringLength,
        i => readValue(i, stringLength))(in)
    }))
  
  private def readValue(in: Input, stringLength: Int): String = 
    new String(in.consume(stringLength), StandardCharsets.US_ASCII)
}

object NilTermFormat extends TermFormat[Nil.type] {
  val tag: Byte = 106 
  
  override def write(out: Output, value: Nil.type): Output = out.put(tag)
  override def read(in: Input): Nil.type =  {
    in.consume(1)
    Nil
  }
}

class ListTermFormat[T](private val elementFormat: TermFormat[T]) extends TermFormat[List[T]] {
  override def write(out: Output, value: List[T]): Output = {
    ListTermFormat.writeHeader(out, value.length)
    value.foreach(elementFormat.write(out, _))
    NilTermFormat.write(out, Nil)
    out
  }

  override def read(in: Input): List[T] = {
    val elements = ListTermFormat.readLength(in)
    val readList = (0 until elements).map(_ => elementFormat.read(in)).toList
    NilTermFormat.read(in)
    readList
  }
}
object ListTermFormat {
  val tag = 108.toByte
  
  def readLength(in: bert.format.io.Input): Int = {
    ByteBuffer.wrap(in.consume(5).drop(1)).getInt()
  }
  
  def writeHeader(out: bert.format.io.Output, length: Int): Output = {
    out.put(tag)
       .put(ByteBuffer.allocate(4).putInt(length).array())
  }
}

class MapTermFormat[K,V](private val keyFormat: TermFormat[K],
                         private val valueFormat: TermFormat[V]) extends TermFormat[Map[K,V]] {
  override def write(out: Output, value: Map[K,V]): Output = {
    out.put(MapTermFormat.tag)
       .put(ByteBuffer.allocate(4).putInt(value.size).array())
    value.foreach({case (key, value) =>
      keyFormat.write(out, key)
      valueFormat.write(out, value)
    })
    out
  }
  override def read(in: Input): Map[K,V] = {
    val elements = ByteBuffer.wrap(in.consume(5).drop(1)).getInt()
    ((0 until elements).foldLeft(Map[K,V]())((map, _) => map + (keyFormat.read(in) -> valueFormat.read(in))))
  }
}
object MapTermFormat {
  val tag: Byte = 116
}