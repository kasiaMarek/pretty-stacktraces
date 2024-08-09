package org.virtuslab.stacktraces.coreq

import org.virtuslab.stacktraces.model.PrettyException
import tastyquery.Contexts.*
import tastyquery.Contexts
import dotty.tools.dotc.classpath.ClassPathFactory
import java.nio.file.Path
import dotty.tools.dotc.util.ClasspathFromClassloader
import tastyquery.jdk.ClasspathLoaders
import tastyquery.Symbols.ClassSymbol
import tastyquery.Trees.DefDef
import org.virtuslab.stacktraces.tastyq.TypesSupport
import org.virtuslab.stacktraces.model.ElementType
import org.virtuslab.stacktraces.model.PrettyStackTraceElement
import org.virtuslab.stacktraces.model.PrettyErrors
import tastyquery.Names
import tastyquery.Traversers.TreeTraverser
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import tastyquery.Trees.Tree
import org.virtuslab.stacktraces.transform.StacktracesCompressor
import scala.util.Try

object Stacktraces:
  def convertToPrettyStackTrace(e: Throwable, classpath: List[Path]): PrettyException =
    val cp = ClasspathLoaders.read(classpath)
    given Context = Context.initialize(cp)
    // println(e.getStackTrace().mkString("\n\t"))
    val st = filterInternalStackFrames(e.getStackTrace)
    // val ll = ctx.findStaticClass("scala.collection.immutable.LazyList")
    // val l = ctx.findStaticClass("java.io.LineNumberReader")
    val h = ctx.findStaticClass("org.virtuslab.stacktraces.tests.nestedLambdas.NestedLambdas")
    // st.map{ elem =>
    //   println(elem.getClassName())
    //   println(elem.getFileName())
    //   println(elem.getMethodName())
    //   println(elem.getModuleName())
    // }
    // treeInfo(ll)
    // treeInfo(l)
    // treeInfo(h)
    // println(h.declarations)
    
    // println("KUPA")
    val inspector = new StacktracesInspector
    val pst = inspector.foreachStacktrace(st)(using LambdaUnraveler(Nil))
    val wrapped = StacktracesCompressor.compressStackTrace(pst)
    PrettyException(e, wrapped)

  // private def treeInfo(cls: ClassSymbol) =
  //   cls.tree match
  //     case Some(t) =>
  //       val o = t.pos.sourceFile.path
  //       val i = t.pos.startLine
  //       println("AAA")
  //     case _ =>
  //       println(s"${cls.displayFullName}: KUPA")

  // TODO: wft is this even ?
  private def filterInternalStackFrames(st: Array[StackTraceElement]): List[StackTraceElement] =
    st.sliding(2).toList.flatMap {
      case Array(fs, sc) =>
        if sc.getMethodName.contains("$adapted") then Nil else List(fs)
    } :+ st.last

class StacktracesInspector(using Context):
  val ts = new TypesSupport

  private def label(d: DefDef): ElementType =  d.symbol match
    //case s if s.flags.is(Flags.ExtensionMethod) => ElementType.ExtensionMethod
    case s if s.name.toString == "$anonfun" => 
      
      val ownerName = s.owner.name.toString
      val parent = if ownerName == "$anonfun" then "some outer lambda" else ownerName
      ElementType.Lambda(ts.toLambda(d), parent)
    case _ => ElementType.Method
        
  private def createPrettyStackTraceElement(d: DefDef, lineNumber: Int)(using ste: StackTraceElement): PrettyStackTraceElement =
                                                      //TODO: this strip should be adjusted
    val nameWithoutPrefix = d.pos.sourceFile.path.stripPrefix("out/bootstrap/stdlib-bootstrapped/scala-3.1.3-RC2-bin-SNAPSHOT-nonbootstrapped/src_managed/main/scala-library-src/")
    PrettyStackTraceElement(ste, label(d), d.name.toString(), nameWithoutPrefix, lineNumber)

  private def createErrorWhileBrowsingTastyFiles(error: PrettyErrors)(using ste: StackTraceElement): PrettyStackTraceElement =
    PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getFileName, ste.getLineNumber, error = Some(error))

  class PossibleDefDefs(ste: StackTraceElement) extends TreeTraverser:
    val defs = new ListBuffer[DefDef]()
    override def traverse(tree: Tree): Unit =
      tree match 
        case d: DefDef => 
          if d.pos.startLine + 1 <= ste.getLineNumber && d.pos.endLine + 1 >= ste.getLineNumber then
            defs += d
        case _ =>
      super.traverse(tree)

    def apply(tree: Tree) =
      traverse(tree)
      val res = defs.result()
      defs.clear()
      res
  end PossibleDefDefs

  private def processDefDefs(
    defdefs: List[DefDef],
    optionalName: Option[String] = None
  )(
    using lambdaUnraveler: LambdaUnraveler, 
    ste: StackTraceElement
  ): (Option[PrettyStackTraceElement], LambdaUnraveler) =
    val decoded = Names.termName(optionalName.getOrElse(ste.getMethodName)).name.toString() // should be decoded (?)
    decoded match
      case d if d.contains("$anonfun$") =>
        val lambdas = defdefs.filter(f => f.name.toString() == "$anonfun")
        val (defdef, newLambdaUnraveler) = lambdaUnraveler.getNextLambdaAndState(lambdas, decoded)
        defdef match
          case Some(head) =>
            (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), newLambdaUnraveler)
          case None =>
            (Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.InlinedLambda)), newLambdaUnraveler)
      case d =>
        defdefs.filter(_.name.toString() != "$anonfun") match
          case Nil =>
            (None, lambdaUnraveler)
          case head :: Nil =>
            (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), lambdaUnraveler)
          case _ => 
            val fun = defdefs.filter(_.name.toString() == d)
            fun match // This will probably fail for nested inline functions, though we cannot disambiguate them
              case head :: Nil =>
                (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), lambdaUnraveler)
              case _ =>
                val extraSuffix = """(.*)\$(\d*)""".r
                decoded match
                  case extraSuffix(name, suffix) =>
                    processDefDefs(defdefs, Some(name))
                  case _ =>
                    (Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.Unknown)), lambdaUnraveler)

  def processStackTrace(ste: StackTraceElement)(using LambdaUnraveler): (LambdaUnraveler, Option[PrettyStackTraceElement]) =
    val className = ste.getClassName
    val symbol =
      try {
        val res = //TODO:: fix this
          if className.last == '$' then ctx.findStaticModuleClass(className.dropRight(1))
          else ctx.findStaticClass(ste.getClassName.stripSuffix("$"))
        Some(res)
      } catch {
        case e: Throwable =>
          None
      }

    symbol.flatMap(_.tree) match
      case Some(tree) =>
        given StackTraceElement = ste
        val defdefs = PossibleDefDefs(ste)(tree)
        val (pse, newLambdaUnraveler) = processDefDefs(defdefs)
        (newLambdaUnraveler, pse)
      case None =>
          val elem = PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber, isTasty = false)
          (summon[LambdaUnraveler], Some(elem))

  
  def foreachStacktrace(st: List[StackTraceElement])(using LambdaUnraveler) =
    st.foldLeft((List.empty[PrettyStackTraceElement], summon[LambdaUnraveler])){ case ((elems, lambdaUnraveler), ste) =>
      val (newLambdaUnraveler, elem) = processStackTrace(ste)(using lambdaUnraveler)
      (elem.toList ++ elems, newLambdaUnraveler)
    }._1.reverse