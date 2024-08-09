package org.virtuslab.stacktraces
package tastyq

import collection.JavaConverters._
import tastyquery.Contexts.Context
import tastyquery.Trees.*
import tastyquery.Types.Type
import tastyquery.Types.*
import tastyquery.Definitions
import tastyquery.Symbols.Symbol
import tastyquery.Contexts.ctx

class TypesSupport(using Context):

  def toLambda(d: DefDef): String =
    s"(${d.paramLists.collectFirst {
      case Left(vls) => vls.map(vd => asSignature(vd.tpt)).mkString(", ")
    }.getOrElse("")}) => ${asSignature(d.resultTpt)}"

  def asSignature(tpeTree: TypeTree): String = inner(tpeTree.toType)

  private def text(str: String): String = str

  private def texts(str: String): String = text(str)

  private def link(symbol: Symbol): String =
    val suffix = if symbol.isTerm  && !symbol.asTerm.isModuleVal then texts(".type") else ""
    symbol.name.toString()

  private def commas(lists: List[String]) = lists match
    case List(single) => single
    case other => other.reduce((r, e) => r ++ texts(", ") ++ e)

  private def isRepeatedAnnotation(term: TermTree) =
    term.tpe match
      case t: TypeRef => t.name.toString() == "Repeated" && t.prefix.match
        case t: ThisType if t.tref.name.toString() == "internal" => true
        case _ => false
      case _ => false

  private def isRepeated(typeRepr: Type) =
    typeRepr match
      case t: TypeRef => t.name.toString() == "<repeated>" && t.prefix.match
        case t: ThisType if t.tref.name.toString() == "scala" => true
        case _ => false
      case _ => false

  // TODO #23 add support for all types signatures that makes sense
  private def inner(tp: TypeMappable): String =

    def noSupported(name: String): String =
      println(s"WARN: Unsupported type: $name: ${tp.showBasic}")
      text(s"Unsupported[$name]")

    tp match
      case t: OrType => inner(t.first) ++ texts(" | ") ++ inner(t.second)
      case t: AndType => inner(t.first) ++ texts(" & ") ++ inner(t.second)
      case t: ByNameType => text("=> ") + inner(t.resultType)
      case t: ConstantType => texts(t.value.toString)
      case t: ThisType => inner(t.tref)
      case t: AnnotatedType =>
        t.typ match
          case at: AppliedType if at.args.size == 1 && isRepeatedAnnotation(t.annotation.tree) => 
            inner(at.args(0)) + text("*") // TODO:: support widcards
          case tpe => inner(tpe)
      case at: AppliedType if isRepeated(at.tycon) =>
        inner(at.tycon) + text("*")
      case tl : TypeLambda =>
        texts("[") ++ commas(tl.paramNames.zip(tl.paramTypeBounds).map { (name, typ) =>
          val normalizedName = if name.toString().matches("_\\$\\d*") then "_" else name.toString()
          texts(normalizedName) ++ inner(typ)
        }) ++ texts("]")
        ++ texts(" =>> ")
        ++ inner(tl.resultType)


      case r: RefinedType => { //(parent, name, info)
        def getRefinementInformation(t: Type): List[Type] = t match {
          case r: RefinedType => getRefinementInformation(r.parent) :+ r
          case t => List(t)
        }

        def getParamBounds(t: PolyType): String = commas(
          t.paramNames.zip(t.paramTypeBounds.map(inner(_)))
            .map(b => texts(b(0).toString()) ++ b(1))
        )

        def getParamList(m: MethodType): String =
          texts("(")
          + m.paramNames.zip(m.paramTypes).map{ case (name, tp) => texts(s"$name: ") ++ inner(tp)}
            .reduceLeftOption((acc: String, elem: String) => acc ++ texts(", ") ++ elem).getOrElse(List())
          ++ texts(")")

        def parseRefinedElem(name: String, info: TypeMappable, polyTyped: String = ""): String = ( info match {
          case m: MethodType => {
            val paramList = getParamList(m)
            texts(s"def $name") ++ polyTyped ++ paramList ++ texts(": ") ++ inner(m.resultType) // ???
          }
          case t: PolyType => {
            val paramBounds = getParamBounds(t)
            val parsedMethod = parseRefinedElem(name, t.resultType)
            if (!paramBounds.isEmpty){
              parseRefinedElem(name, t.resultType, texts("[") ++ paramBounds ++ texts("]"))
            } else parseRefinedElem(name, t.resultType)
          }
          case bnt: ByNameType => texts(s"def $name: ") ++ inner(tp)
          case t: TypeBounds => texts(s"type $name") ++ inner(t)
          case t: TypeRef => texts(s"val $name: ") ++ inner(t)
          case t: TermRef => texts(s"val $name: ") ++ inner(t)
          case other => noSupported(s"Not supported type in refinement $info")
        } ) ++ texts("; ")

        // def parsePolyFunction(info: Type): String = info match {
        //   case t: PolyType =>
        //     val paramBounds = getParamBounds(t)
        //     val method = t.resultType.asInstanceOf[MethodType]
        //     val paramList = getParamList(method)
        //     val resType = inner(method.resultType)
        //     texts("[") ++ paramBounds ++ texts("] => ") ++ paramList ++ texts(" => ") ++ resType
        //   case other => noSupported(s"Not supported type in refinement $info")
        // }
        val refinementInfo = getRefinementInformation(r)
        val refinedType = refinementInfo.head
        val refinedElems = refinementInfo.tail.collect{ case r: RefinedType => r }.toList
        val prefix: String = 
          if refinedType.isSameType(ctx.defn.ObjectType) then inner(refinedType) + texts(" ") else ""
        // if (refinedType.typeSymbol.fullName == "scala.PolyFunction" && refinedElems.size == 1) {
        //   parsePolyFunction(refinedElems.head.info)
        // } else {
        prefix + texts("{ ") ++ refinedElems.flatMap(e => parseRefinedElem(e.refinedName.toString(), e)) + texts(" }")
        //}
      }
      case at: AppliedType =>
        import dotty.tools.dotc.util.Chars._
        val name = inner(at.tycon)
        val arity = at.args.size
        if !name.forall(isIdentifierPart) && at.args.size == 2 then
          inner(at.args.head)
          ++ texts(" ")
          ++ name
          ++ texts(" ")
          ++ inner(at.args.last)
        else if at.isSubType(TypeRef(NoPrefix, ctx.defn.FunctionNClass(arity - 1).asType)) then
          at.args match
            case Nil =>
              ""
            case Seq(rtpe) =>
              text("() => ") + inner(rtpe)
            case Seq(arg, rtpe) =>
              val partOfSignature = arg match
                case byName: ByNameType => texts("(") ++ inner(byName) ++ texts(")")
                case _ => inner(arg)
              partOfSignature ++ texts(" => ") ++ inner(rtpe)
            case args =>
              texts("(") ++ commas(args.init.map(inner)) ++ texts(") => ") ++ inner(args.last)
        else if at.isSubType(ctx.defn.TupleType) then
          at.args match
            case Nil =>
              ""
            case args =>
              texts("(") ++ commas(args.map(inner)) ++ texts(")")
        else inner(at.tycon) ++ texts("[") ++ commas(at.args.map { t => t match
          case _: WildcardTypeArg => texts("_") ++ inner(t)
          case _ => inner(t)
        }) ++ texts("]")

      case tp: TypeRef =>
        def name =  tp.optSymbol.map(link(_)).getOrElse("")
        tp.prefix match {
          case _: ThisType => texts(s"this.$name")
          case _ => name
        }
      case tr: TermRef =>
        tr.optSymbol.flatMap(_.tree) match
          case Some(vd: ValDef) => inner(vd.tpt.toType)
          case _          => tr.optSymbol.map(link(_)).getOrElse("")

      case tb: TypeBounds =>
        if(tb.low == tb.high) texts(" = ") ++ inner(tb.low)
        else typeBoundsTreeOfHigherKindedType(tb.low, tb.high)

      case NoPrefix => ""

      case mt: MatchType => //(bond, sc, cases) =>
        val casesTexts = mt.cases.flatMap { mc =>
          val cases = mc.paramTypeBounds.map(inner).mkString(" | ")
          texts("  case ") ++ cases ++ texts(" => ") ++ inner(mc.result) ++ texts("\n")
        }
        inner(mt.scrutinee) + texts(" match {\n") + casesTexts + texts("}")
      case tp => tp.showBasic

  private def typeBound(t: Type, low: Boolean): String =
    val ignore = if(low) t.isSameType(ctx.defn.NothingType)  else t.isSameType(ctx.defn.AnyType)
    val prefix = text(if low then " >: " else " <: ")
    t match {
      case l: TypeLambda => prefix + texts("(") ++ inner(l) ++ texts(")")
      case p: ParamRef => prefix + inner(p)
      case other if !ignore => prefix + inner(other)
      case _ => ""
    }

  private def typeBoundsTreeOfHigherKindedType(low: Type, high: Type): String =
    def regularTypeBounds(low: Type, high: Type): String =
      typeBound(low, low = true) + typeBound(high, low = false)
    high.match
      case tl: TypeLambda =>
        if tl.resultType.isSameType(ctx.defn.AnyType) then
          texts("[") ++ commas(tl.paramNames.zip(tl.paramTypeBounds).map { (name, typ) =>
            val normalizedName = if name.toString().matches("_\\$\\d*") then "_" else name.toString()
            texts(normalizedName) ++ inner(typ)
          }) ++ texts("]")
        else
          regularTypeBounds(low, high)
      case _ => regularTypeBounds(low, high)
