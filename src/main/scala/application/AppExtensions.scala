package application

import futures.TryExtensions.{errorHandling, functions}

import scala.collection.mutable
import scala.util.Try

object AppExtensions {

  /**
   * Convert args of application to Map.
   *
   * @param args      Application arguments as scala Array
   * @param separator Argument separator
   * @return Args as Map
   * @see <a href="https://coderwall.com/p/4l73-a/scala-fold-foldleft-and-foldright">Scala fold, foldLeft, and foldRight</a>
   * @note The application arguments are of type Array de String de Java. To use this function it is necessary to
   *       convert it to a Scala Array [javaArray.toArray = scalaArray]
   */
  def argsAppToMap(args: Array[String], separator: String): Try[Map[String, String]] = Try {
    val accumulator = mutable.Map[String, String]()
    args.foldLeft(accumulator)((acc, arg) => {
      if (arg.contains(separator)) {
        val argsSplit = arg.split(separator)
        acc += (argsSplit.head -> argsSplit.last)
      } else acc
    }).toMap
  }.onFailure(t => new Exception(s"Fatal error:: Description: ${t.getMessage}")).addError()

}
