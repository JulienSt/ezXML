package indv.jstengel.ezxml.extension.ct


import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.Elem
import indv.jstengel.ezxml.extension.ct.CompileTimeReflectHelper.{isSimple, mapNameAsExpr}

import scala.language.higherKinds



// https://stackoverflow.com/questions/18450203/retrieve-the-name-of-the-value-a-scala-macro-invocation-will-be-assigned-to
object CTConverter {
    
    /* ============================================ Iterator Conversion ============================================ */
    
    def xml[A, I[_] <: IterableOnce[_]] (i : I[A]): Elem = macro convertSeqImpl[A, I]
    def convertSeqImpl[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                                   (i : c.Expr[I[A]])
                                                   (implicit ITag: c.WeakTypeTag[I[A]],
                                                             ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertSeq(c)(i, None, None)
    
    /* --- */
    
    def xml[A, I[_] <: IterableOnce[_]] (i : I[A], fieldName : String): Elem = macro convertSeqAsFieldImpl[A, I]
    def convertSeqAsFieldImpl[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                                          (i : c.Expr[I[A]], fieldName : c.Expr[String])
                                                          (implicit ITag: c.WeakTypeTag[I[A]],
                                                                    ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertSeq(c)(i, None, Some(fieldName))
    
    /* --- */
    
    def xml[A, I[_] <: IterableOnce[_]] (i : I[A],
                                                mapFieldNames : (String, String) => Option[String]): Elem =
        macro convertSeqWithMappingImpl[A, I]
    def convertSeqWithMappingImpl[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                                              (i             : c.Expr[I[A]],
                                                               mapFieldNames : c.Expr[(String, String) => Option[String]])
                                                              (implicit ITag: c.WeakTypeTag[I[A]],
                                                                        ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertSeq(c)(i, Some(mapFieldNames), None)
    
    /* --- */
    
    def xml[A, I[_] <: IterableOnce[_]] (i             : I[A],
                                                mapFieldNames : (String, String) => Option[String],
                                                fieldName     : String) : Elem = macro convertSeqWithAllImpl[A, I]
    def convertSeqWithAllImpl[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                                          (i             : c.Expr[I[A]],
                                                           mapFieldNames : c.Expr[(String, String) => Option[String]],
                                                           fieldName     : c.Expr[String])
                                                          (implicit ITag: c.WeakTypeTag[I[A]],
                                                                    ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertSeq(c)(i, Some(mapFieldNames), Some(fieldName))
    
    /* --- */
    
    def convertSeq[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                               (i             : c.Expr[I[A]],
                                                mapFieldNames : Option[c.Expr[(String, String) => Option[String]]],
                                                fieldName     : Option[c.Expr[String]])
                                               (implicit ITag : c.WeakTypeTag[I[A]],
                                                         ATag : c.WeakTypeTag[A]): c.Expr[Elem] = { import c.universe._
//        println("iterable: " + ATag.tpe.typeSymbol.fullName)
        val lType = ITag.tpe
        val fullTypeName = lType.typeSymbol.fullName
        if ( ATag.tpe <:< ITag.tpe ) { /* prohibit StackOverflow at compile time */
            val typeAsExpr = c.Expr[String](q"$fullTypeName")
            val mappedName = mapNameAsExpr(c)(mapFieldNames, fieldName, typeAsExpr)
            createRuntimeConversion(c)(i, mappedName)
        } else {
            val typeAsExpr = c.Expr[String](q"""$fullTypeName + "[" + ${ATag.tpe.typeSymbol.fullName} + "]" """)
            c.Expr[Elem](q"""
                scala.xml.Elem(${mapNameAsExpr(c)(mapFieldNames, fieldName, typeAsExpr)},
                               $typeAsExpr,
                               scala.xml.Null,
                               scala.xml.TopScope,
                               false,
                               $i.iterator.map(e => indv.jstengel.ezxml.extension.ct.CTConverter.xml(e)).toSeq: _*)
            """) // todo todo include mapping
        }
    }
    
    /* ============================================= Array Conversion ============================================== */
    
    def xml[A] (l: Array[A]): Elem = macro convertArrayImpl[A]
    def xml[A] (l: Array[A], fieldName: String): Elem = macro convertArrayAsFieldImpl[A]
    
    def convertArrayImpl[A] (c: blackbox.Context)
                            (l: c.Expr[Array[A]])
                            (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] = convertArray(c)(l, None)
    
    def convertArrayAsFieldImpl[A] (c: blackbox.Context)
                                   (l: c.Expr[Array[A]], fieldName: c.Expr[String])
                                   (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] = convertArray(c)(l, Some(fieldName))
    
    def convertArray[A](c: blackbox.Context)
                       (l: c.Expr[Array[A]], fieldName: Option[c.Expr[String]])
                       (implicit ATag: c.WeakTypeTag[Array[A]]): c.Expr[Elem] = { import c.universe._
//        println("array: " + ATag.tpe.typeSymbol.fullName)
        c.Expr[Elem](q"""
            scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                           ${ATag.tpe.typeSymbol.fullName},
                           scala.xml.Null,
                           scala.xml.TopScope,
                           false,
                           $l.map(e => indv.jstengel.ezxml.extension.ct.CTConverter.xml(e)).toIndexedSeq: _*)
        """) // todo include mapping
    }
    
    /* =========================================== Arbitrary Conversion ============================================ */
    
    def xml[A] (a: A): Elem = macro convertImpl[A]
    def xml[A] (a: A, mapFieldNames: (String, String) => Option[String]): Elem = macro convertWithMappingImpl[A]
    def xml[A] (a: A, fieldName: String): Elem = macro convertWithFieldImpl[A]
    def xml[A] (a             : A,
                       mapFieldNames : (String, String) => Option[String],
                       fieldName     : String) : Elem = macro convertWithAllImpl[A]
    
    def convertImpl[A] (c: blackbox.Context)
                       (a: c.Expr[A])
                       (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, None, None)
    
    def convertWithMappingImpl[A] (c : blackbox.Context)
                                  (a : c.Expr[A], mapFieldNames: c.Expr[(String, String) => Option[String]])
                                  (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, Some(mapFieldNames), None)
    
    def convertWithFieldImpl[A] (c : blackbox.Context)
                                (a : c.Expr[A], fieldName: c.Expr[String])
                                (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, None, Some(fieldName))
    
    def convertWithAllImpl[A] (c : blackbox.Context)
                              (a             : c.Expr[A],
                               mapFieldNames : c.Expr[(String, String) => Option[String]],
                               fieldName     : c.Expr[String])
                              (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] =
        convertClassToXML(c)(a, Some(mapFieldNames), Some(fieldName))
    
    
    def convertClassToXML[A] (c : blackbox.Context)
                             (a             : c.Expr[A],
                              mapFieldNames : Option[c.Expr[(String, String) => Option[String]]],
                              fieldName     : Option[c.Expr[String]])
                             (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] = { import c.universe._
              
        val aType = ATag.tpe
        val fullTypeName = aType.typeSymbol.fullName
        val typeAsExpr = c.Expr[String](q"$fullTypeName")
        val mappedName = mapNameAsExpr(c)(mapFieldNames, fieldName, typeAsExpr)
        
        if (isSimple(c)(aType))
            c.Expr[Elem](q"""
                scala.xml.Elem($mappedName, $typeAsExpr, scala.xml.Null, scala.xml.TopScope, true, Seq(): _*) %
                    scala.xml.Attribute("value", scala.xml.Text($a.toString()), scala.xml.Null)
            """)

            //todo array
//        else if (aType <:< typeOf[Array[_]]) {
//            val (symbol, tparam) = aType match {
//                case TypeRef(_, symbol, tparam::Nil) => (symbol.fullName, tparam.typeSymbol.fullName)
//            }
//            c.Expr[Elem](q"""
//            scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
//                           $symbol[$tparam],
//                           scala.xml.Null,
//                           scala.xml.TopScope,
//                           false,
//                           $a.map(e => indv.jstengel.ezxml.extension.ct.CTConverter.xml(e)).toIndexedSeq: _*)
//            """) // todo mapping
//
//        }
        
        else if (aType.typeSymbol.isAbstract)
            createRuntimeConversion(c)(a, mappedName)
        
        else {
            c.Expr[Elem](
                aType.decls
                     .collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
                     .get
                     .paramLists
                     .head
                     .foldLeft(q"""scala.xml.Elem($mappedName,
                                                  $typeAsExpr,
                                                  scala.xml.Null,
                                                  scala.xml.TopScope,
                                                  true,
                                                  Seq(): _*)""") { case (quote, field : Symbol) =>
                         val fName = TermName(field.name.decodedName.toString)
                         
                         // todo annotations
                         // todo check which annotations can be used together
                         //  pattern matching through all annotations, since they can be combined
//                         val annotations = field.annotations.flatMap(_.tree match {
//                             case q"new $tpname($param)" if isValid(tpname.toString()) => Some(tpname, Some(param))
//                             case q"new $tpname"         if isValid(tpname.toString()) => Some(tpname, None)
//                             case _ => None
//                         })
//                         println("---------------------- Annotations ----------------------")
//                         annotations.foreach(println)
//                         println("^^^^^^^^^^^^^^^^^^^^^^ Annotations ^^^^^^^^^^^^^^^^^^^^^^")
                         
                         val fieldCall =
                             if(CompileTimeReflectHelper.isMacroCallingEnclosingClass(c, fullTypeName))
                                 q"$fName"
                             else
                                 q"$a.$fName"
                         
                         if (isSimple(c)(field.typeSignature))
                             q"""$quote % scala.xml.Attribute(${ field.typeSignature.typeSymbol.fullName.toString },
                                                              ${ fName.toString },
                                                              scala.xml.Text($fieldCall.toString),
                                                              scala.xml.Null)"""
                         else
                             q"""val scala.xml.Elem(prefix, label, attribs, scope, child @ _*) = $quote
                                 scala.xml.Elem(prefix, label, attribs, scope, false, child ++
                                 indv.jstengel.ezxml.extension.ct.CTConverter.xml($fieldCall,${fName.toString}): _*)"""
                     }
            )
        }
    }
    
    //    def createRuntimeConversion[A] (c : blackbox.Context)
//                                   (a : c.Expr[A],
//                                    mapFieldNames: Option[c.Expr[(String, String) => Option[String]]],
//                                    fieldName : Option[c.Expr[String]])
//                                   (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] = { import c.universe._
//        val mapping = mapFieldNames.getOrElse( c.Expr[(String, String) => Option[String]](
//            q"(_: String, _: String) => None")
//        )
//        c.Expr[Elem](q"""
//            indv.jstengel.ezxml.extension.ct.RTConverter.convertToXML($a, $mapping,
//            ${fieldName.getOrElse(c.Expr[String](q"null"))})
//        """)
//    }
    def createRuntimeConversion[A] (c : blackbox.Context)
                                   (a : c.Expr[A],
                                    fieldName : c.Expr[String])
                                   (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] = { import c.universe._
        c.Expr[Elem](q"""indv.jstengel.ezxml.extension.rt.RTConverter.convertToXML($a, pre = $fieldName)""")
    }
}
