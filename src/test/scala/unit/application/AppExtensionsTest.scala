package unit.application

import application.AppExtensions.argsAppToMap
import org.scalatest.flatspec.AnyFlatSpec

trait AppExtensionsTestConstants {

  val PARAM_I = "keyI=valueI"
  val PARAM_II = "keyII-valueII"
  val PARAM_III = "keyIII:valueIII"
  val PARAM_IV = "keyIV="
  val PARAM_V = "=valueV"

}

class AppExtensionsTest extends AnyFlatSpec with AppExtensionsTestConstants {

  "Args App with correct argument" should "execute getArgsAppAsMap and return arguments as map" in {
    val argsApp = Array(PARAM_I)

    val result = argsAppToMap(argsApp, "=")

    assert(result.isSuccess)
    assert(result.getOrElse("Error").isInstanceOf[Map[String, String]])
    assert(result.get.size == 1)
    assert(result.get.contains("keyI"))
    assert(result.get("keyI").equals("valueI"))
  }

  "Args App with correct and incorrect arguments" should "execute getArgsAppAsMap and return empty Map" in {
    val argsApp = Array(PARAM_I, PARAM_II, PARAM_III, PARAM_IV, PARAM_V)

    val result = argsAppToMap(argsApp, "=")

    assert(result.isSuccess)
    assert(result.getOrElse("Error").isInstanceOf[Map[String, String]])
    assert(result.get.size == 3)
    assert(result.get("").equals("valueV"))
    assert(result.get("keyIV").equals("keyIV"))
    assert(result.get("keyI").equals("valueI"))
  }

  "Args App with incorrect argument" should "execute getArgsAppAsMap and return empty Map" in {
    val argsApp = Array(PARAM_II)

    val result = argsAppToMap(argsApp, ":")

    assert(result.isSuccess)
    assert(result.getOrElse("Error").isInstanceOf[Map[String, String]])
    assert(result.get.isEmpty)
  }

  "Args App with incorrect arguments" should "execute getArgsAppAsMap and return empty Map" in {
    val argsApp = Array(PARAM_I, PARAM_II, PARAM_III)

    val result = argsAppToMap(argsApp, ",")

    assert(result.isSuccess)
    assert(result.getOrElse("Error").isInstanceOf[Map[String, String]])
    assert(result.get.isEmpty)
  }

  "Args App as Map" should "execute getOrElseArgApp and return value expected" in {
    val argsApp = Array(PARAM_I, PARAM_IV, PARAM_V)

    val argsAsMap = argsAppToMap(argsApp, "=")

    var result: String = argsAsMap.get.getOrElse("keyI", "Error")

    assert(result.nonEmpty)
    assert(result.equals("valueI"))

    result = argsAsMap.get.getOrElse("keyIV", "Error")

    assert(result.nonEmpty)
    assert(result.equals("keyIV"))

    result = argsAsMap.get.getOrElse("", "Error")

    assert(result.nonEmpty)
    assert(result.equals("valueV"))
  }

  it should "execute getOrElseArgApp and return default value" in {
    val argsApp = Array(PARAM_I, PARAM_II, PARAM_IV, PARAM_V)

    val argsAsMap = argsAppToMap(argsApp, "/")

    var result: String = argsAsMap.get.getOrElse("keyI", "Error")

    assert(result.nonEmpty)
    assert(result.equals("Error"))

    result = argsAsMap.get.getOrElse("keyII", "Error")

    assert(result.nonEmpty)
    assert(result.equals("Error"))

    result = argsAsMap.get.getOrElse("keyIV", "Error")

    assert(result.nonEmpty)
    assert(result.equals("Error"))

    result = argsAsMap.get.getOrElse("", "Error")

    assert(result.nonEmpty)
    assert(result.equals("Error"))

  }
}
