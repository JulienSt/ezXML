package indv.jstengel.ezxml.extension.ct

import indv.jstengel.ezxml.extension.XmlObjectTrait
import indv.jstengel.ezxml.extension.ct.CompileTimeReflectHelper._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.Elem


object CtDecoder {
    
    def obj[A]: Elem => A = macro objImpl[A]
        
    /**
     * the implementation of macro obj
     * this function creates a loading function to extract
     * @param c
     * @param ATag
     * @tparam A
     * @return
     */
    def objImpl[A](c : blackbox.Context)(implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem => A] = { import c.universe._
    
        /**
         * creates a string representation for a given type, such that it can be loaded through RtDecoder.load,
         * CtDecoder.obj, or getTypeFromString
         * Sadly, this needs to be a nested function and can not be generalize, due to compiler problems
         * @param t the type that will be converted to a string
         * @return a String representation for type t in the for of t[typeParams]
         */
        def createStringRepresentation (t : Type)(typeParams : List[Type] = getTypeParams(c)(t)): String = {
            val symbol = t.typeSymbol
            if (symbol.isAbstract && t.baseClasses.length == 1)
                symbol.name.toString
            else if (typeParams.isEmpty)
                     symbol.fullName
            else
                s"${symbol.fullName}[${typeParams.map(t => createStringRepresentation(t)()).mkString(",")}]"
        }
    
        val aType                 = ATag.tpe
        val aTypeSymbol           = aType.typeSymbol
        val typeParams            = getTypeParams(c)(aType)
        val companion             = aTypeSymbol.asClass.companion
        val companionSig          = companion.typeSignature
        val isCalledFromCompanion = c.enclosingClass.symbol == companion
        /* I know that c.enclosingClass is deprecated, but that is the best way to check, if the macro was called
         * from the companion object. */
        
        if (!isCalledFromCompanion && companionSig <:< typeOf[XmlObjectTrait])
            c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => $companion.${TermName("loadFromXML")}(elem)""")

        else if (aTypeSymbol.isModuleClass){
            c.Expr[Elem => A](q"""(elem: scala.xml.Elem) =>
                ${aTypeSymbol.owner.typeSignature.member(aTypeSymbol.name.toTermName)}""")
        }
        
        else if(isObject(c)(aType, companionSig)){
            c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => $companion""")
        }
        
        else if (isSimple(c)(aType))
            if (aType <:< c.typeOf[String])
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")""")
            else
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")${ tagToFunctionCall(c)(ATag) }""")
        
        else if (aType <:< typeOf[Array[_]] || isConstructedThroughIterable(c)(aType, isCalledFromCompanion)) {
            val tparam = aType match {
                case TypeRef(_, _, tparam::Nil) => tq"$tparam"
                case TypeRef(_, _, tps)         => tq"(..$tps)"
            }
            val companionMembers = companionSig.members
            val tree = companionMembers
                .collectFirst{
                    case m : MethodSymbol if {val name = m.name.toString; name == "from" || name == "fromSeq"} =>
                        q"""(elem: scala.xml.Elem) => {$m( elem.child.map{case (e: scala.xml.Elem) =>
                            indv.jstengel.ezxml.extension.ct.CtDecoder.obj[$tparam](e)}).asInstanceOf[$aType]}"""
                }
                .getOrElse{
                    companionMembers.collectFirst{case m : MethodSymbol if m.name.toString == "apply" =>
                        val fType = m.paramLists.head.head.typeSignature
                        val (_, _, isRepeated) = getActualFieldType(c)(fType, Map(), createStringRepresentation(_)())
                        if (isRepeated)
                            q"""(elem: scala.xml.Elem) => $m( elem.child.map{case (e: scala.xml.Elem) =>
                                indv.jstengel.ezxml.extension.ct.CtDecoder.obj[$tparam](e)}: _*)"""
                        else
                            q"""(elem: scala.xml.Elem) => $m( elem.child.map{case (e: scala.xml.Elem) =>
                                indv.jstengel.ezxml.extension.ct.CtDecoder.obj[$tparam](e)})"""
                    }.get
                }
            c.Expr[Elem => A](tree)
        }

        else if (aType.typeSymbol.isAbstract)
            c.Expr[Elem => A](q"""(e: scala.xml.Elem) => indv.jstengel.ezxml.extension.rt.RtDecoder.load[$aType](e)""")
            
        else {
            val (constructor, typeMap, _) = getConstructorWithTypeMap(c)(aType, typeParams)
            val paramLists = constructor.paramLists
    
            if ( paramLists.forall(_.isEmpty) )
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => new $aType()""")

            else if (!isCalledFromCompanion && constructor.isPrivate)
                c.Expr[Elem => A](q"""(e: scala.xml.Elem) =>
                    indv.jstengel.ezxml.extension.rt.RtDecoder.load[$aType](e)""")
                
            else {
                val treeAsString = paramLists
                    .map{ paramList =>
                        val lastIndex = paramList.length - 1
                        paramList.zipWithIndex
                                 .foldLeft("("){ case (quote : String, (field : Symbol, index : Int)) =>
                                     val fName           = field.name.decodedName.toString
                                     val (fieldType, fieldTypeAsString, isRepeated) =
                                         getActualFieldType(c)(field.typeSignature,
                                                               typeMap,
                                                               createStringRepresentation(_)())
                                     
                                     val fieldAsQuote    =
                                         if ( isSimple(c)(fieldType) )
                                             s"""(elem.attributes
                                                |     .collectFirst{
                                                |         case scala.xml.PrefixedAttribute(_,
                                                |                                          "$fName",
                                                |                                          scala.xml.Text(value),
                                                |                                          _) => value
                                                |     }
                                                |     .get)${
                                                 if ( fieldType <:< c.typeOf[String] )
                                                     ""
                                                 else
                                                     "." + tagToFunctionCall(c)(c.WeakTypeTag(fieldType))}
                                             |""".stripMargin
                                         else
                                             s"""indv.jstengel
                                             |    .ezxml
                                             |    .extension
                                             |    .ct
                                             |    .CtDecoder
                                             |    .obj[${fieldTypeAsString}](elem
                                             |    .child
                                             |    .collectFirst{case c: scala.xml.Elem if c.prefix == "$fName" => c}
                                             |    .get)${if(isRepeated) ":_*" else ""}""".stripMargin
                                     
                                     if ( index < lastIndex )
                                         s"$quote$fieldAsQuote, "
                                     else
                                         s"$quote$fieldAsQuote)"
                        }
                    }
                    .foldLeft(s"(elem: scala.xml.Elem) => new ${aType.typeSymbol.fullName}"){
                        case (quote : String, listAsQuote : String) => s"$quote$listAsQuote"
                    }
//                println(treeAsString)
                c.Expr[Elem => A](c.parse(treeAsString))
            }
        }
    }
    
    /**
     *
     * @param c
     * @tparam A
     * @return
     */
    def tagToFunctionCall[A] (c : blackbox.Context) : PartialFunction[c.WeakTypeTag[A], c.TermName] = {
        import c.universe.TermName
        import c.{WeakTypeTag, typeOf}
        {
            case WeakTypeTag(t) if t <:< typeOf[Int]     || t <:< typeOf[java.lang.Integer]   => TermName("toInt")
            case WeakTypeTag(t) if t <:< typeOf[Double]  || t <:< typeOf[java.lang.Double]    => TermName("toDouble")
            case WeakTypeTag(t) if t <:< typeOf[Long]    || t <:< typeOf[java.lang.Long]      => TermName("toLong")
            case WeakTypeTag(t) if t <:< typeOf[Float]   || t <:< typeOf[java.lang.Float]     => TermName("toFloat")
            case WeakTypeTag(t) if t <:< typeOf[Short]   || t <:< typeOf[java.lang.Short]     => TermName("toShort")
            case WeakTypeTag(t) if t <:< typeOf[Byte]    || t <:< typeOf[java.lang.Byte]      => TermName("toByte")
            case WeakTypeTag(t) if t <:< typeOf[Boolean] || t <:< typeOf[java.lang.Boolean]   => TermName("toBoolean")
            case WeakTypeTag(t) if t <:< typeOf[Char]    || t <:< typeOf[java.lang.Character] => TermName("head")
            case WeakTypeTag(t) if t <:< typeOf[String]                                       => TermName("")
        }
    }
    
}

