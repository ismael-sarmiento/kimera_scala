package unit.futures

import futures.TryExtensions
import futures.TryExtensions.{errorHandling, functions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

trait TryExtensionsTestConstants {

  // constants
  val ERROR_MESSAGE_I = "Intentionally caused error I"
  val ERROR_MESSAGE_II = "Intentionally caused error II"
  val ENRICHMENT_MESSAGE = " - Enrichment Message"
  val SUCCESS_MESSAGE = "Success Message"
  val SUCCESS_MESSAGE_VALUE = "getException failed on Success object"
  val failureMessageI: Failure[Exception] = Failure(new Exception(ERROR_MESSAGE_I))
  val failureMessageIEnriched: Failure[Exception] = Failure(new Exception(ERROR_MESSAGE_I + ENRICHMENT_MESSAGE))
  val failureSuccessMessage: Failure[Exception] = Failure(new ClassCastException(SUCCESS_MESSAGE_VALUE))
  val successObject: Success[String] = Success(SUCCESS_MESSAGE)
  val expectedErrors = List(failureSuccessMessage, failureMessageIEnriched, SUCCESS_MESSAGE_VALUE)

  def myErrors: String => ListBuffer[Failure[Throwable]] = TryExtensions.getErrors

  def trashErrors: ListBuffer[Failure[Throwable]] = TryExtensions.getTrashErrors

  def allErrors: Iterable[Failure[Throwable]] = TryExtensions.getAllErrors

  // functions
  def printException: Throwable => Unit = { e => println(e.getMessage) }

  def enrichmentException: Throwable => Throwable = { e => new Exception(e.getMessage + ENRICHMENT_MESSAGE) }
}

class TryExtensionsCoreTest extends AnyFlatSpec with TryExtensionsTestConstants with BeforeAndAfterAll {

  "A Failure Object" should "execute the onFailure function, show error message in console and return failure object" in {

    val result = failureMessageI.onFailure(printException)

    assert(result.isFailure)
    assert(result.getOrElse(ERROR_MESSAGE_II).toString === ERROR_MESSAGE_II)
  }

  it should "execute the onFailure function, enrichment exception and return failure object" in {

    val result = failureMessageI.onFailure(enrichmentException)

    assert(result.isFailure)
    assert(result.getOrElse(ERROR_MESSAGE_II).toString === ERROR_MESSAGE_II)
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
    assert(result.getOrElse(ERROR_MESSAGE_I) === SUCCESS_MESSAGE)
  }

}

class TryExtensionsErrorHandlingTest extends AnyFlatSpec with TryExtensionsTestConstants with BeforeAndAfterAll {

  // Init - Class
  override protected def beforeAll: Unit = {
    super.beforeAll()
    TryExtensions.cleanAllErrors
    assert(trashErrors.isEmpty)
  }

  "A Failure Object" should "execute the onFailure function, enrichment exception, add error and return failure object" in {

    val result = failureMessageI.addError().onFailure(enrichmentException).addError()

    assert(result.isFailure)
    assert(result.getOrElse(ERROR_MESSAGE_II).toString === ERROR_MESSAGE_II)
    assert(trashErrors.nonEmpty)
    // exists: method takes a predicate function and will use it to find the first element in the collection which matches the predicate.
    assert(trashErrors.exists(_.toString === failureMessageIEnriched.toString))
  }

  "A Success Object" should "execute the onFailure function, not show error message in console, get exception, add error (general) and return failure object" in {

    def printException: Throwable => Unit = { e => println(e.getMessage) }

    val result = successObject.onFailure(printException).getException.addError()

    assert(result.isFailure)
    result.onFailure(e => assert(e.getMessage === "getException failed on Success object"))
    assert(trashErrors.nonEmpty)
    assert(trashErrors.exists(_.toString contains "getException"))
  }

  it should "execute the onFailure function, not show error message in console, get exception, add error (particular) and return failure object" in {

    assert(myErrors(successObject.get).isEmpty)

    def printException: Throwable => Unit = { e => println(e.getMessage) }

    val result = successObject.onFailure(printException).getException.addError(successObject.get)

    assert(result.isFailure)
    result.onFailure(e => assert(e.getMessage === "getException failed on Success object"))
    assert(myErrors(successObject.get).nonEmpty)
    assert(myErrors(successObject.get).exists(_.toString contains "getException"))
  }

  // Finish - Class
  override protected def afterAll: Unit = {
    super.afterAll()
    assert(validateTrashErrors)
    assert(validateAllErrors)
    TryExtensions.cleanAllErrors
    assert(allErrors.isEmpty)
  }

  private def validateAllErrors: Boolean = {
    // all errors
    assert(allErrors.nonEmpty)
    assert(allErrors.size === 4)
    // forall: returns true if all elements in the collection such that p(x) is true.
    expectedErrors.map(_.toString) forall allErrors.toString().contains
  }

  private def validateTrashErrors: Boolean = {
    // trash errors
    assert(trashErrors.nonEmpty)
    assert(trashErrors.size === 3)
    // forall: returns true if all elements in the collection such that p(x) is true.
    expectedErrors.map(_.toString) forall trashErrors.toString().contains
  }

}
