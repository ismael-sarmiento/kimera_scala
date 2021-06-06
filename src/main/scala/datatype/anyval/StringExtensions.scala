package datatype.anyval

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import scala.language.implicitConversions

object StringExtensions extends App {

  // extensions
  implicit def stringFunctions[T](src: String): Functions[T] = Functions(src)

}


case class Functions[T](src: String) {

  def toInputStream: InputStream = new ByteArrayInputStream(src.getBytes(StandardCharsets.UTF_8.name))
}