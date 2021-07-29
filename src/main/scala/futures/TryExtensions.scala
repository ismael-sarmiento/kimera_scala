package futures

import futures.TryExtensions.{CLASS_CAST_EXCEPTION_MESSAGE, TRASH}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}


object TryExtensions {

  implicit def errorHandlingIterable[T](src: Iterable[Try[T]]): ErrorsHandling[T] = ErrorsHandling(src)

  // constants
  val TRASH = "trash"
  val CLASS_CAST_EXCEPTION_MESSAGE = "getException failed on Success object"

  // extensions
  implicit def functions[T](src: Try[T]): Core[T] = Core(src)

  implicit def errorHandling[T](src: Try[T]): ErrorHandling[T] = ErrorHandling(src)

  // attributes
  private implicit val _errors: mutable.Map[String, ListBuffer[Failure[Throwable]]] = mutable.Map[String, ListBuffer[Failure[Throwable]]]()

  // functions
  def getErrors(key: String): ListBuffer[Failure[Throwable]] = {
    if (_errors.contains(key))
      _errors(key)
    else {
      _errors.update(key, EMPTY)
      EMPTY
    }
  }

  def EMPTY: ListBuffer[Failure[Throwable]] = ListBuffer[Failure[Throwable]]()

  def getTrashErrors: ListBuffer[Failure[Throwable]] = {
    if (_errors.contains(TRASH)) _errors(TRASH)
    else {
      _errors.update(TRASH, EMPTY)
      EMPTY
    }
  }

  def getAllErrors: Iterable[Failure[Throwable]] = _errors.values.flatten

  def cleanErrors(key: String): Boolean = if (_errors.get(key).map(_.clear).isDefined) true else false

  def cleanTrashErrors: Boolean = if (_errors.get(TRASH).map(_.clear).isDefined) true else false

  def cleanAllErrors: Boolean = if (Option(_errors.map(_._2.clear())).isDefined) true else false
}

case class ErrorHandling[T](src: Try[T])(implicit _errors: mutable.Map[String, ListBuffer[Failure[Throwable]]]) {

  def addError(key: String): Try[T] = {
    src match {
      case Success(_) => src
      case Failure(e) =>
        _errors.get(key) match {
          case Some(value) => value.addOne(Failure(e))
          case None => _errors.update(TRASH, ListBuffer(Failure(e)))
        }
        Failure(e)
    }
  }

  def addError(): Try[T] = {
    src match {
      case Success(_) => src
      case Failure(e) =>
        _errors.get(TRASH) match {
          case Some(value) => value.addOne(Failure(e))
          case None => _errors.update(TRASH, ListBuffer(Failure(e))) // update return Unit VS put return values of map
        }
        Failure(e)
    }
  }
}

case class ErrorsHandling[T](src: Iterable[Try[T]])(implicit _errors: mutable.Map[String, ListBuffer[Failure[Throwable]]]) {

  def addErrors(key: String): Iterable[Try[T]] = src.map(s => ErrorHandling(s).addError(key)) // TODO: missing test

  def addError(): Iterable[Try[T]] = src.map(s => ErrorHandling(s).addError()) // TODO: missing test

  def getValidElements: Iterable[T] = src.filter(_.isSuccess).map(_.get) // TODO: missing test

  def getInvalidElements: Iterable[Failure[T]] = src.filter(_.isFailure).asInstanceOf[Iterable[Failure[T]]] // TODO: missing test
}

case class Core[T](src: Try[T]) {

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
      case Success(_) => Failure(new ClassCastException(CLASS_CAST_EXCEPTION_MESSAGE))
      case Failure(e) => Success(e)
    }
  }
}