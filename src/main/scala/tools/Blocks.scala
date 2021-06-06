package tools

import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

object Blocks {

  private val LOGGER: Logger = LoggerFactory.getLogger(getClass.getName)

  def Time[R](block: => R): Try[R] = Try {
    val tInit = System.currentTimeMillis()
    val result = block // call-by-name
    val tEnd = System.currentTimeMillis()
    LOGGER.debug(s"Execution time: ${tEnd - tInit} ms")
    result
  }
}
