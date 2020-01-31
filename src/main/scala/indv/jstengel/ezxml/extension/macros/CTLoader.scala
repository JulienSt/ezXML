package indv.jstengel.ezxml.extension.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.Elem
import indv.jstengel.ezxml.extension.macros.CompileTimeReflectHelper.{isSimple, mapNameAsExpr}


object CTLoader {
    
    def obj[A]: Elem => A = macro loadObjFromXml[A]
    def loadObjFromXml[A](c : blackbox.Context)
                         (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem => A] = { import c.universe._
        
        val aType = ATag.tpe
        
        if (isSimple(c)(aType))
            if (aType <:< c.typeOf[String])
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")""")
            else
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => (elem \@ "value")${ tagToName(c)(ATag) }""")
            
        else if (aType.typeSymbol.isAbstract)
            c.Expr[Elem => A](q"""(elem: scala.xml.Elem) =>
                indv.jstengel.ezxml.extension.reflection.RTLoader.load[$aType](elem)
            """)
        else {
            
            val paramLists = aType.decls
                                  .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                                  .get
                                  .paramLists
    
            if ( paramLists.forall(_.isEmpty) )
                c.Expr[Elem => A](q"""(elem: scala.xml.Elem) => new $aType()""")
    
            else {
                val treeAsString =
                    paramLists
                        .map{ paramList =>
                            val lastIndex = paramList.length - 1
                            paramList.zipWithIndex
                                     .foldLeft("("){ case (quote : String, (field : Symbol, index : Int)) =>
                                         val fName     = field.name.decodedName.toString
                                         val fieldType = field.typeSignature
                                         val fieldAsQuote =
                                             if ( isSimple(c)(fieldType) )
                                                 s"""(elem.attributes
                                                    |     .collectFirst{case scala.xml
                                                    |                             .PrefixedAttribute(_,
                                                    |                                                $fName,
                                                    |                                                scala.xml.Text
                                                    |                                                (value),
                                                    |                                                _) => value
                                                    |                  }
                                                    |     .get)${
                                                     if ( fieldType <:< c.typeOf[String] )
                                                         ""
                                                     else
                                                         "." + tagToName(c)(c.WeakTypeTag(fieldType))
                                                 }""".stripMargin
                                             else
                                                 s"""indv.jstengel
                                                 |    .ezxml
                                                 |    .extension
                                                 |    .macros
                                                 |    .CTLoader
                                                 |    .obj[$fieldType](elem
                                                 |    .child
                                                 |    .collectFirst{case c: scala.xml.Elem if c.prefix=="$fName" => c}
                                                 |    .get)""".stripMargin
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
//                val expr = c.Expr[Elem => A](q"${ c.parse(treeAsString) }")
                c.Expr[Elem => A](q"${ c.parse(treeAsString) }")
            }
        }
    }
    
    
    def tagToName[A] (c : blackbox.Context) : PartialFunction[c.WeakTypeTag[A], c.TermName] = {
        import c.{WeakTypeTag, typeOf}
        import c.universe.TermName
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

