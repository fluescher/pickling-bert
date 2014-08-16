package bert

import bert.format._
import bert.format.io.ArrayInput._
import bert.format.io._

import scala.annotation.implicitNotFound
import scala.util.{Success, Try}

object Bert {
  type Term = Array[Byte]
  final val PROTOCOL_VERSION = 131.toByte

  @implicitNotFound("No member of type class TermLike in scope for ${T}")
  trait TermLike[T] {
    def toTermValue(t: T): Term = termFormat.write(new ArrayOutput(), t).toArray
    def fromTermValue(data: Term): T = termFormat.read(data)
    def termFormat: TermFormat[T]
  }
  object TermLike {
    implicit object TermLikeInt extends TermLike[Int] {
      override def termFormat: TermFormat[Int] = IntTermFormat
    }
    implicit object TermLikeDouble extends TermLike[Double] {
      override def termFormat: TermFormat[Double] = DoubleTermFormat
    }
    implicit object TermLikeString extends TermLike[String] {
      override def termFormat: TermFormat[String] = StringTermFormat
    }
    implicit def termLikeMap[K,V](implicit termLikeK: TermLike[K], termLikeV: TermLike[V]): TermLike[Map[K,V]] = new TermLike[Map[K,V]] {
      override def termFormat: TermFormat[Map[K,V]] = new MapTermFormat(implicitly[TermLike[K]].termFormat,
                                                                        implicitly[TermLike[V]].termFormat)
    }
    implicit def termLikeList[T: TermLike]: TermLike[List[T]] = new TermLike[List[T]] {
      override def termFormat: TermFormat[List[T]] = new ListTermFormat(implicitly[TermLike[T]].termFormat)
    }
  }

  def toBert[T: TermLike](v: T): Term =
    Array[Byte](PROTOCOL_VERSION) ++ implicitly[TermLike[T]].toTermValue(v)

  def fromBert[T: TermLike](data: Term): Try[T] =
    readWithChecks(data.drop(1))

  private def toTerm[T: TermLike](v: T): Term =
    implicitly[TermLike[T]].toTermValue(v)
    
  private def readWithChecks[T: TermLike](term: Term): Try[T] =
    Success(term).map(implicitly[TermLike[T]].fromTermValue)

  sealed class BertException(message: String = "") extends RuntimeException(message)
  case class TermTooShort() extends BertException
  case class InvalidTag(tag: Int) extends BertException(s"Invalid tag: ${tag.toString}")

}
