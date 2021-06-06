package datatype.anyref

import futures.TryExtensions.errorHandling

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Try

object IterableExtensions {

  // extensions
  implicit def genericFunctions[T <: Iterable[Any]](src: T): GenericFunctions[T] = GenericFunctions(src)

  implicit def tryFunctions[U <: Iterable[Try[Any]]](src: U): TryFunctions[U] = TryFunctions(src)

}


case class GenericFunctions[T](src: Iterable[Any]) {

  def filterByType[U](implicit tag: ClassTag[U]): Iterable[Any] = {
    src.flatMap {
      case element@tag(_: U) => Some(element)
      case _ => None
    }

  }
}

case class TryFunctions[U](src: Iterable[Try[Any]]) {

  def getSuccessValues[K]: Iterable[K] = src.filter(_.isSuccess).map(_.get.asInstanceOf[K])

  def getFailureValues: Iterable[Try[Any]] = src.filter(_.isFailure)

  def addErrors(): Iterable[Try[Any]] = src.filter(_.isFailure).map(_.addError())

}