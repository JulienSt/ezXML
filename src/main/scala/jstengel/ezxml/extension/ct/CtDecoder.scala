package jstengel.ezxml.extension.ct

import jstengel.ezxml.extension.XmlObjectTrait
import jstengel.ezxml.extension.ct.CompileTimeReflectHelper.{
    getTypeParams,
    isObject,
    isSimple,
    isConstructedThroughIterable,
    getConstructorWithTypeMap,
    getActualFieldType
}
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.Elem


//noinspection DuplicatedCode
object CtDecoder {
    
    /**
     * this macro creates a function to load an object from an [[Elem]], that is of type [[A]]
     *
     * @note When you use [[mutable.Buffer]] as type-parameter in obj[T],
     *       this macro does not compile for some unknown reason.
     *       You can instead use mutable.ArrayBuffer as type-parameter. That will compile correctly.
     *       If you do need to load mutable.Buffer however, the RtDecoder will load everything correctly
     *
     * @tparam A the type that the resulting function will be able to load.
     * @return a function that can load an object of type [[A]] from an [[Elem]]
     */
    def obj[A]: Elem => A = macro objImpl[A]
    
    /**
     * the implementation of macro obj
     * this function creates a decoding function to extract an object of type [[A]] from an [[Elem]]
     *
     * @note When you use mutable.Buffer as type-parameter in obj[T],
     *       this macro does not compile for some unknown reason.
     *       You can instead use mutable.ArrayBuffer as type-parameter. That will compile correctly.
     *       If you do need to load mutable.Buffer however, the RtDecoder will load everything correctly
     *
     * @param c context, to access types and symbols, during compile time
     *          (this is automatically added by calling the macro)
     * @param ATag the WeakTypeTag of [[A]], this is automatically filled in by the compiler
     * @tparam A the type that the resulting function will be able to load.
     * @return a Expression of a function that can load an object of type [[A]] from an [[Elem]]
     */
    def objImpl[A](c : blackbox.Context)(implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem => A] = {
    
        import c.universe.{MethodSymbol, Quasiquote, Symbol, TermName, Type, TypeRef, typeOf}
        
        /**
         * creates a string representation for a given type, such that it can be loaded through RtDecoder.load,
         * CtDecoder.obj, or getTypeFromString
         * Sadly, this needs to be a nested function and can not be generalize, due to compiler problems,
         * therefor this function is duplicated in CtEncoder
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
            
        else if (aTypeSymbol.isModuleClass)
            c.Expr[Elem => A]( q"""(elem: scala.xml.Elem) =>
                    ${aTypeSymbol.owner.typeSignature.member(aTypeSymbol.name.toTermName)}""")
        
        else if(isObject(c)(aType, companionSig))
            c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => $companion""")
        
        else if (isSimple(c)(aType))
            if (aType <:< typeOf[String])
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")""")
            else
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")${ tagToFunctionCall(c)(ATag) }""")

//        else if (aType <:< typeOf[Product] && !aTypeSymbol.isAbstract) {
//            companionSig.members.collectFirst{ case apply : MethodSymbol if apply.name.toString == "apply" =>
//                val paramsLists = apply.paramLists
//                val applyTParams = getTypeParams(c)(apply.returnType)
//                val typeMap      = applyTParams.zip(typeParams).toMap
//                val elem = TermName("elem")
//                val loadedParamLists = paramsLists.map{ paramList =>
//                    val loadedFields = paramList.map{ field =>
//                        val fName = field.name.encodedName.toString
//                        val (tpe, _, isRepeated) =
//                            getActualFieldType(c)(field.typeSignature, typeMap, createStringRepresentation(_)())
//                        if(isSimple(c)(tpe)) {
//                            if (tpe <:< typeOf[String])
//                                q"""$elem.attributes.collectFirst{
//                                        case scala.xml.PrefixedAttribute(_, $fName, scala.xml.Text(value), _) =>
//                                            value
//                                    }.get"""
//                            else
//                                 q"""$elem.attributes.collectFirst{
//                                       case scala.xml.PrefixedAttribute(_, $fName, scala.xml.Text(value), _) =>
//                                           value
//                                     }.get.${tagToFunctionCall(c)(c.WeakTypeTag(tpe))}"""
//                        } else if (isRepeated) {
//                            q"""jstengel.ezxml.extension.ct.CtDecoder.obj[$tpe](
//                                    $elem.child
//                                         .collectFirst{ case c: scala.xml.Elem if c.prefix == $fName => c }
//                                         .get):_*"""
//                        } else
//                            q"""jstengel.ezxml.extension.ct.CtDecoder.obj[$tpe](
//                                    $elem.child
//                                         .collectFirst{ case c: scala.xml.Elem if c.prefix == $fName => c }
//                                         .get)"""
//                    }
//                    loadedFields
//                }
//                c.Expr[Elem => A](q"""($elem: scala.xml.Elem) => $apply(...$loadedParamLists)""")
//            }.get
//        }
        
        else if ( aType <:< typeOf[Array[_]] || isConstructedThroughIterable(c)(aType, isCalledFromCompanion) ) {
            val tparam = aType match {
                case TypeRef(_, _, tparam::Nil) => tq"$tparam"
                case TypeRef(_, _, tps)         => tq"(..$tps)"
            }
            val companionMembers = companionSig.members
            val tree = companionMembers
                .collectFirst{
                    case m : MethodSymbol if {val name = m.name.toString; name == "from" || name == "fromSeq"} =>
                        q"""(elem: scala.xml.Elem) => {$m( elem.child.map{case (e: scala.xml.Elem) =>
                            jstengel.ezxml.extension.ct.CtDecoder.obj[$tparam](e)}).asInstanceOf[$aType]}"""
                }
                .getOrElse{
                    companionMembers.collectFirst{case apply : MethodSymbol if apply.name.toString == "apply" =>
                        q"""(elem: scala.xml.Elem) => $apply( elem.child.map{case (e: scala.xml.Elem) =>
                            jstengel.ezxml.extension.ct.CtDecoder.obj[$tparam](e)}: _*)"""
                    }.get
                }
            println(tree)
            c.Expr[Elem => A](tree)
        }

        else if (aType.typeSymbol.isAbstract)
            c.Expr[Elem => A](q"""(e: scala.xml.Elem) => jstengel.ezxml.extension.rt.RtDecoder.load[$aType](e)""")
        
        else {
            val (constructor, typeMap, _) = getConstructorWithTypeMap(c)(aType, typeParams)
            val paramLists = constructor.paramLists
    
            if ( paramLists.forall(_.isEmpty) )
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => new $aType()""")

            else if (!isCalledFromCompanion && constructor.isPrivate)
                c.Expr[Elem => A](q"""(e: scala.xml.Elem) =>
                    jstengel.ezxml.extension.rt.RtDecoder.load[$aType](e)""")
                
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
                                                 if ( fieldType <:< typeOf[String] )
                                                     ""
                                                 else
                                                     "." + tagToFunctionCall(c)(c.WeakTypeTag(fieldType))}
                                             |""".stripMargin
                                         else
                                             s"""jstengel.ezxml.extension.ct.CtDecoder.obj[$fieldTypeAsString](
                                                |    elem.child
                                                |        .collectFirst{
                                                |            case c: scala.xml.Elem if c.prefix == "$fName" => c
                                                |        }
                                                |        .get)${if(isRepeated) ":_*" else ""}""".stripMargin
                                     
                                     if ( index < lastIndex )
                                         s"$quote$fieldAsQuote, "
                                     else
                                         s"$quote$fieldAsQuote)"
                        }
                    }
                    .foldLeft(s"(elem: scala.xml.Elem) => new ${aType.typeSymbol.fullName}"){
                        case (quote : String, listAsQuote : String) => s"$quote$listAsQuote"
                    }
                c.Expr[Elem => A](c.parse(treeAsString))
            }
        }
    }
    
    /**
     * This function is used to load a simple value from a string value (refer to [[isSimple()]] for more information
     * on the definition of simple type
     * @param c context, to access types and symbols, during compile time
     * @tparam A the type that will be loaded from the string
     * @return a partial function that associates a WeakTypeTag with a method call from the String-class
     */
    def tagToFunctionCall[A] (c : blackbox.Context) : PartialFunction[c.WeakTypeTag[A], c.TermName] = {
        import c.universe.{TermName, typeOf, WeakTypeTag}
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

