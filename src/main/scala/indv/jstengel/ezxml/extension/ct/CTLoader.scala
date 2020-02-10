package indv.jstengel.ezxml.extension.ct

import indv.jstengel.ezxml.extension.ct.CompileTimeReflectHelper.{getTypeParams, isConstructedThroughIterable, isSimple}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.Elem


object CTLoader {
    
    def obj[A]: Elem => A = macro objImpl[A]
    def objImpl[A](c : blackbox.Context)(implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem => A] =
        loadObjFromXml(c)(isCalledFromAnnotation = false)
    
    def annotationObj[A]: Elem => A = macro annotationObjImpl[A]
    def annotationObjImpl[A](c : blackbox.Context)(implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem => A] =
        loadObjFromXml(c)(isCalledFromAnnotation = true)
        
    /**
     * the implementation of macro obj
     * this function creates a loading function to extract
     * @param c
     * @param ATag
     * @tparam A
     * @return
     */
    def loadObjFromXml[A](c : blackbox.Context)
                         (isCalledFromAnnotation : Boolean)
                         (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem => A] = { import c.universe._
    
        /**
         * creates a string representation for a given type, such that it can be loaded through RTLoader.load,
         * CTLoader.obj, or getTypeFromString
         * Sadly, this needs to be a nested function and can not be generalize, due to compiler problems
         * @param t the type that will be converted to a string
         * @param typeParams the type params that will be included in the string representation of t
         * @return a String representation for type t in the for of t[typeParams]
         */
        def createStringRepresentation (t : Type)
                                       (typeParams : List[Type] = getTypeParams(c)(t)): String =
            if (typeParams.isEmpty)
                t.typeSymbol.fullName
            else
                s"${t.typeSymbol.fullName}[${typeParams.map(t => createStringRepresentation(t)()).mkString(",")}]"
        
        val aType = ATag.tpe
        
        if (isSimple(c)(aType))
            if (aType <:< c.typeOf[String])
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")""")
            else
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")${ tagToFunctionCall(c)(ATag) }""")

        else if (aType <:< typeOf[Array[_]] || isConstructedThroughIterable(c)(aType, isCalledFromAnnotation)) {
            val (symbol, tparam) = aType match {
                case TypeRef(_, symbol, tparam::Nil) => (symbol.fullName, createStringRepresentation(tparam)())
                case TypeRef(_, symbol, tps)         => (symbol.fullName, "(" + tps.map(_.toString).mkString(",") + ")")
            }
            val treeAsString =
                s"""(elem: scala.xml.Elem) =>
                   |    $symbol(elem.child.map{case (e: scala.xml.Elem) =>
                   |        indv.jstengel.ezxml.extension.ct.CTLoader.obj[$tparam](e)}:_*)
                   |    """.stripMargin
            c.Expr[Elem => A](c.parse(treeAsString))
        }

        else if (aType.typeSymbol.isAbstract)
            c.Expr[Elem => A](q"""(e: scala.xml.Elem) => indv.jstengel.ezxml.extension.rt.RTLoader.load[$aType](e)""")
            
        else {
            
            val constructor = aType.decls
                                   .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                                   .get
            val typeMap = getTypeParams(c)(constructor.returnType).zip(getTypeParams(c)(aType)).toMap
            val paramLists = constructor.paramLists
    
            if ( paramLists.forall(_.isEmpty) )
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => new $aType()""")
    
            else {
                val treeAsString =
                    paramLists
                        .map{ paramList =>
                            val lastIndex = paramList.length - 1
                            paramList.zipWithIndex
                                     .foldLeft("("){ case (quote : String, (field : Symbol, index : Int)) =>
                                         val fName           = field.name.decodedName.toString
                                         val fieldType       = field.typeSignature
                                         val actualFieldType = typeMap.getOrElse(fieldType, fieldType)
                                         val isRepeated      = fieldType.toString.endsWith("*")
                                         val fieldAsQuote    =
                                             if ( isSimple(c)(actualFieldType) )
                                                 s"""(elem.attributes
                                                    |     .collectFirst{
                                                    |         case scala.xml.PrefixedAttribute(_,
                                                    |                                          "$fName",
                                                    |                                          scala.xml.Text(value),
                                                    |                                          _) => value
                                                    |     }
                                                    |     .get)${
                                                     if ( actualFieldType <:< c.typeOf[String] )
                                                         ""
                                                     else
                                                         "." + tagToFunctionCall(c)(c.WeakTypeTag(actualFieldType))}
                                                 |""".stripMargin
                                             else // todo "else if repeated" has to be added in this chain
                                                 s"""indv.jstengel
                                                 |    .ezxml
                                                 |    .extension
                                                 |    .ct
                                                 |    .CTLoader
                                                 |    .obj[${
                                                     if(isRepeated)
                                                         s"Seq[${actualFieldType.toString.dropRight(1)}]" // todo problems here
                                                     else
                                                         fieldType
                                                 }](elem
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

