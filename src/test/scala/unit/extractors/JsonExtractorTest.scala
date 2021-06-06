package unit.extractors

import java.io.InputStream

import datatype.anyval.StringExtensions.stringFunctions
import extractors.JsonExtractor
import org.scalatest.flatspec.AnyFlatSpec

// basic.json
case class BasicModel(keyI: String, keyII: Char, keyIII: Int, keyIV: Float, keyV: Boolean, keyVI: Boolean, keyVII: Boolean)

// advancedI.json
case class AdvancedModelI(userI: Array[AnyVal], userII: Array[UserII])

case class UserII(sub_keyI: Boolean, sub_keyII: List[AnyVal], sub_keyIII: Float)

// advancedII.json
case class AdvancedModelII(userI: List[AnyVal], userII: (UserII, UserIII))

case class UserIII(sub_keyI: Boolean, sub_keyII: List[AnyVal], sub_keyIII: List[AnyVal])

trait JsonExtractorTestConstant {
  val basicInputStream: InputStream =
    """
      |{
      |  "keyI": "valueI",
      |  "keyII": "C",
      |  "keyIII": 123,
      |  "keyIV": 10.0,
      |  "keyV": "true",
      |  "keyVI": 0,
      |  "keyVII": true
      |}
      |""".stripMargin.toInputStream
  val basicResourceJSON = "/json/basic.json"
  val advancedIResourceJSON = "/json/advancedI.json"
  val advancedIIResourceJSON = "/json/advancedII.json"
}

class JsonExtractorTest extends AnyFlatSpec with JsonExtractorTestConstant {

  "A InputStream (Basic)" should "execute JsonExtractor.load function and load successfully" in {

    val basicModel = JsonExtractor().load[BasicModel](basicInputStream)

    assert(basicModel.keyI.isInstanceOf[String])
    assert(basicModel.keyI.equals("valueI"))

    assert(basicModel.keyII.isInstanceOf[Char])
    assert(basicModel.keyII.equals('C'))

    assert(basicModel.keyIII.isInstanceOf[Int])
    assert(basicModel.keyIII.equals(123))

    assert(basicModel.keyIV.isInstanceOf[Float])
    assert(basicModel.keyIV == 10.0)

    assert(basicModel.keyV.isInstanceOf[Boolean])
    assert(basicModel.keyV.equals(true))

    assert(basicModel.keyVI.isInstanceOf[Boolean])
    assert(basicModel.keyVI.equals(false))

    assert(basicModel.keyVII.isInstanceOf[Boolean])
    assert(basicModel.keyVII.equals(true))

  }

  "A Path Resource (Basic) as String" should "execute JsonExtractor.load function and load its respective file (Json) successfully" in {

    val basicModel = JsonExtractor().load[BasicModel](basicResourceJSON)

    assert(basicModel.keyI.isInstanceOf[String])
    assert(basicModel.keyI.equals("valueI"))

    assert(basicModel.keyII.isInstanceOf[Char])
    assert(basicModel.keyII.equals('C'))

    assert(basicModel.keyIII.isInstanceOf[Int])
    assert(basicModel.keyIII.equals(123))

    assert(basicModel.keyIV.isInstanceOf[Float])
    assert(basicModel.keyIV == 10.0)

    assert(basicModel.keyV.isInstanceOf[Boolean])
    assert(basicModel.keyV.equals(true))

    assert(basicModel.keyVI.isInstanceOf[Boolean])
    assert(basicModel.keyVI.equals(false))

    assert(basicModel.keyVII.isInstanceOf[Boolean])
    assert(basicModel.keyVII.equals(true))
  }

  "A Path Resource (AdvancedI) as String" should "execute JsonExtractor.load function and load its respective file (Json) successfully" in {

    val advancedIModel = JsonExtractor().load[AdvancedModelI](advancedIResourceJSON)

    val userIExpectedValues = List("valueI", "V", 123, "True", 123.123)
    val userIIExpectedValuesI = List("sub_valueI", "sub_valueII", 10, 12.2, "E")
    val userIIExpectedValuesII = List("W", 12.43, "true")

    assert(advancedIModel.userI.isInstanceOf[Array[AnyVal]])
    assert(advancedIModel.userI.forall(userIExpectedValues.contains))

    assert(advancedIModel.userII.isInstanceOf[Array[UserII]])
    val result = advancedIModel.userII.map {
      case u: UserII =>
        u.sub_keyI.isInstanceOf[Boolean] &&
          (u.sub_keyI || !u.sub_keyI) &&
          u.sub_keyII.isInstanceOf[List[AnyVal]] && (u.sub_keyII.forall(userIIExpectedValuesI.contains) || u.sub_keyII.forall(userIIExpectedValuesII.contains)) &&
          u.sub_keyIII.isInstanceOf[Float] && (u.sub_keyIII == 123.toFloat || u.sub_keyIII == 21.23.toFloat)
      case _ => false
    }

    assert(!result.contains(false))
  }

  "A Path Resource (AdvancedII) as String" should "execute JsonExtractor.load function and load its respective file (Json) successfully" in {

    val advancedIIModel = JsonExtractor().load[AdvancedModelII](advancedIIResourceJSON)

    val userIExpectedValues = List("valueI", "V", 123, "True", 123.123, true, "True", 0, "False", false)
    val userIIExpectedValuesI = List("sub_valueI", "sub_valueII", 10, 12.2, "E")
    val userIIExpectedValuesII = List(12.43, "true")
    val userIIExpectedValuesIII = List(21.23, "valueI", "valueII", false)

    assert(advancedIIModel.userI.isInstanceOf[List[AnyVal]])
    assert(advancedIIModel.userI.forall(userIExpectedValues.contains))

    assert(advancedIIModel.userII.isInstanceOf[(UserII, UserIII)])
    assert(advancedIIModel.userII._1.sub_keyI.isInstanceOf[Boolean] && advancedIIModel.userII._1.sub_keyI)
    assert(advancedIIModel.userII._1.sub_keyII.isInstanceOf[List[AnyVal]] && advancedIIModel.userII._1.sub_keyII.forall(userIIExpectedValuesI.contains))
    assert(advancedIIModel.userII._1.sub_keyIII.isInstanceOf[Float] && advancedIIModel.userII._1.sub_keyIII == 123.toFloat)

    assert(advancedIIModel.userII._2.sub_keyI.isInstanceOf[Boolean] && !advancedIIModel.userII._2.sub_keyI)
    assert(advancedIIModel.userII._2.sub_keyII.isInstanceOf[List[AnyVal]] && advancedIIModel.userII._2.sub_keyII.forall(userIIExpectedValuesII.contains))
    assert(advancedIIModel.userII._2.sub_keyII.isInstanceOf[List[AnyVal]] && advancedIIModel.userII._2.sub_keyIII.forall(userIIExpectedValuesIII.contains))
  }

}
