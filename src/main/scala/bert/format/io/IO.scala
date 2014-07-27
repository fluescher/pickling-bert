package bert.format.io

import java.util.Arrays

import scala.collection.mutable.ArrayBuffer

trait Input {
  def size: Int
  def head: Byte
  def consume(n: Int): Array[Byte]
}
class ArrayInput(private var wrapee: Array[Byte]) extends Input {
  override def size: Int = wrapee.length
  override def head: Byte = wrapee(0)
  override def consume(n: Int): Array[Byte] = {
    val readData = Arrays.copyOfRange(wrapee, 0, n)
    wrapee = wrapee.drop(n)
    readData
  }
}

object ArrayInput {
  import scala.language.implicitConversions
  
  implicit def arrayToInput(arr: Array[Byte]): Input = new ArrayInput(arr)
}

trait Output {
  def put(data: Byte): Output
  def put(data: Array[Byte]): Output
  def toArray: Array[Byte]
}
class ArrayOutput extends Output {
  private val data = new ArrayBuffer[Byte]()

  override def put(newData: Byte): ArrayOutput = { 
    data += newData
    this 
  }
  override def put(newData: Array[Byte]): ArrayOutput = {
    data ++= newData
    this
  }
  override def toArray: Array[Byte] = data.toArray
}

