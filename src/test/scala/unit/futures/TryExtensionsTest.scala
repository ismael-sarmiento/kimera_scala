package unit.futures

import futures.TryExtensions
import futures.TryExtensions.{errorHandling, functions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

trait TryExtensionsTestConstants {

  // constants
  val ErrorMessageI = "Intentionally caused error I"
  val ErrorMessageII = "Intentionally caused error II"
  val EnrichmentMessage = " - Enrichment Message"
  val SuccessMessage = "Success Message"
  val failureMessageI: Failure[Exception] = Failure(new Exception(ErrorMessageI))
  val failureMessageIEnriched: Failure[Exception] = Failure(new Exception(ErrorMessageI + EnrichmentMessage))
  val failureSuccessMessage: Failure[Exception] = Failure(new ClassCastException("getException failed on Success object"))
  val successObject: Success[String] = Success(SuccessMessage)
  val errors: ListBuffer[Failure[Throwable]] = TryExtensions.getErrors
  val expectedErrors = List(failureSuccessMessage, failureMessageIEnriched)

  // functions
  def printException: Throwable => Unit = { e => println(e.getMessage) }

  def enrichmentException: Throwable => Throwable = { e => new Exception(e.getMessage + EnrichmentMessage) }
}

class TryExtensionsTest extends AnyFlatSpec with TryExtensionsTestConstants with BeforeAndAfterAll {


  // Init - Class
  override protected def beforeAll: Unit = {
    TryExtensions.cleanErrors()
    assert(errors.isEmpty)
    super.beforeAll()
  }

  "A Failure Object" should "execute the onFailure function, show error message in console and return failure object" in {

    val result = failureMessageI.onFailure(printException)

    assert(result.isFailure)
    assert(result.getOrElse(ErrorMessageII).toString === ErrorMessageII)
  }

  it should "execute the onFailure function, enrichment exception and return failure object" in {

    val result = failureMessageI.onFailure(enrichmentException)

    assert(result.isFailure)
    assert(result.getOrElse(ErrorMessageII).toString === ErrorMessageII)
  }

  it should "execute the onFailure function, enrichment exception, add error and return failure object" in {

    val result = failureMessageI.onFailure(enrichmentException).addError()

    assert(result.isFailure)
    assert(result.getOrElse(ErrorMessageII).toString === ErrorMessageII)
    assert(errors.nonEmpty)
    // exists: method takes a predicate function and will use it to find the first element in the collection which matches the predicate.
    assert(errors.exists(_.toString === failureMessageIEnriched.toString))
  }

  it should "execute the onFailure function, enrichment exception, get exception and return exception" in {

    val result = failureMessageI.onFailure(enrichmentException).getException

    assert(result.isSuccess)
    result.onFailure(e => assert(e.getMessage === failureMessageIEnriched.toString))
  }

  "A Success Object" should "execute the onFailure function and return success object" in {

    def printException: Throwable => Unit = { e => println(e.getMessage) }

    val result = successObject.onFailure(printException)

    assert(result.isSuccess)
    assert(result.getOrElse(ErrorMessageI) === SuccessMessage)
  }

  it should "execute the onFailure function, not show error message in console, get exception, add error and return failure object" in {

    def printException: Throwable => Unit = { e => println(e.getMessage) }

    val result = successObject.onFailure(printException).getException.addError()

    assert(result.isFailure)
    result.onFailure(e => assert(e.getMessage === "getException failed on Success object"))
    assert(errors.nonEmpty)
    assert(errors.exists(_.toString contains "getException"))
  }

  // Finish - Class
  override protected def afterAll: Unit = {
    assert(errors.nonEmpty)
    assert(errors.size === 2)
    // forall: returns true if all elements in the collection such that p(x) is true.
    assert(expectedErrors.map(_.toString) forall errors.toString().contains)
    TryExtensions.cleanErrors()
    assert(errors.isEmpty)
    super.afterAll()
  }

}
