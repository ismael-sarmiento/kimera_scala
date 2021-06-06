package extractors

import java.io.InputStream

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.reflect.ClassTag
import scala.reflect.io.File

object JsonExtractor {
  private val mapperDefault: JsonMapper = JsonMapper.builder().addModule(DefaultScalaModule).build()

  def apply(mapper: JsonMapper = mapperDefault) = new JsonExtractor(mapper)

}

/**
 * @see <a href="https://github.com/FasterXML/jackson-module-scala">Jackson Module Scala</a>
 */
case class JsonExtractor(mapper: ObjectMapper) {

  def load[T: ClassTag](jsonFilePath: String): T = this.load(File(getClass.getResource(jsonFilePath).getPath))

  def load[T: ClassTag](jsonFile: File): T = this.load(jsonFile.inputStream())

  def load[T: ClassTag](input: InputStream): T = mapper.readValue(input, implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])
}
