package futures

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}


object TryExtensions {

  // extensions
  implicit def functions[T](src: Try[T]): Functions[T] = Functions(src)

  implicit def errorHandling[T](src: Try[T]): ErrorHandling[T] = ErrorHandling(src)

  // attributes
  private implicit val _errors: ListBuffer[Failure[Throwable]] = ListBuffer[Failure[Throwable]]()

  // functions
  def getErrors: ListBuffer[Failure[Throwable]] = _errors

  def cleanErrors(): Unit = _errors.clear()
}

case class ErrorHandling[T](src: Try[T])(implicit _errors: ListBuffer[Failure[Throwable]]) {

  def addError(): Try[T] = {
    src match {
      case Success(_) => src
      case Failure(e) =>
        _errors += Failure(e)
        Failure(e)
    }
  }
}

case class Functions[T](src: Try[T]) {

  def onFailure[U](func: Throwable => U): Try[T] = {
    src match {
      case Success(_) => src
      case Failure(exception) =>
        func(exception) match {
          case t: Throwable => Failure(t)
          case _ => Failure(exception)
        }
    }
  }

  def getException: Try[Throwable] = {
    src match {
      case Success(_) => Failure(new ClassCastException("getException failed on Success object"))
      case Failure(e) => Success(e)
    }
  }
}