package org.virtuslab.stacktraces.core

import org.virtuslab.stacktraces.model.ClasspathWrapper
import org.virtuslab.stacktraces.model.TastyWrapper
import org.virtuslab.stacktraces.model.PrettyException
import org.virtuslab.stacktraces.model.ElementType
import org.virtuslab.stacktraces.io.TastyFilesLocator
import org.virtuslab.stacktraces.transform.StacktracesCompressor

import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File
import java.nio.file.Paths
import tastyquery.Trees.DefDef
import tastyquery.Traversers.TreeTraverser
import org.virtuslab.stacktraces.tasty.TypesSupport
import org.virtuslab.stacktraces.model.PrettyStackTraceElement
import org.virtuslab.stacktraces.model.PrettyErrors

object Stacktraces:

  def convertToPrettyStackTrace(e: Throwable, additionalClasspath: Seq[String] = Nil): PrettyException =
    val tastyFilesLocator = TastyFilesLocator(Thread.currentThread().getContextClassLoader, additionalClasspath)
    val st = filterInternalStackFrames(e.getStackTrace)
    val ctp = tastyFilesLocator.classNameToPath(st.map(_.getClassName))
    val tastyFiles = tastyFilesLocator.tastyFilesFromStackTrace(ctp)
    val pst = StacktracesInspector.inspectStackTrace(st, tastyFiles, ctp)
    val wrapped = StacktracesCompressor.compressStackTrace(pst)
    PrettyException(e, wrapped)


  private def filterInternalStackFrames(st: Array[StackTraceElement]): List[StackTraceElement] =
    st.sliding(2).toList.flatMap {
      case Array(fs, sc) =>
        if sc.getMethodName.contains("$adapted") then Nil else List(fs)
    } :+ st.last
