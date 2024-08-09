package org.virtuslab.stacktraces

import scala.io.Source

import org.virtuslab.stacktraces.coreq.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

import org.junit.Assert.assertTrue
import org.junit.Assert.assertEquals
import org.junit.Test

import java.nio.file.Paths
import org.virtuslab.stacktraces.util.StdClasspath
import org.virtuslab.stacktraces.model.PrettyStackTraceElement
import org.virtuslab.stacktraces.model.PrettyException

trait TestExecutor:

  val test: () => Unit

  @Test
  final def executeTest =
    // Enable test only on JDK 11
    // assumeTrue(System.getProperty("java.version").startsWith("11."))

    val className = this.getClass.getName.split("\\.")
    val expected = Source.fromResource(Paths.get(className.head, className.tail*).toString).mkString

    try
      test()
    catch
      case e: Throwable =>
        val prettyStackTrace = convertToPrettyStackTraceWithStdlib(e)
        val builded = PrettyExceptionPrinter.prettyStacktrace(prettyStackTrace).build
        val lines = expected.split("\n").size
        val result = builded.split("\n").take(lines).mkString("\n").replaceFirst("Exception in thread .*?: ", "").replaceAll("\u001b\\[[;\\d]*m", "")
        assertEquals(expected, result)

  private def convertToPrettyStackTraceWithStdlib(e: Throwable): PrettyException =
    Stacktraces.convertToPrettyStackTrace(e, StdClasspath())
