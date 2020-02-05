package indv.jstengel.ezxml.extension.ct


import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.Elem
import indv.jstengel.ezxml.extension.ct.CompileTimeReflectHelper.isSimple
import indv.jstengel.ezxml.extension.mapping.FieldMappings

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
                                                mappings : FieldMappings): Elem =
        macro convertSeqWithMappingImpl[A, I]
    def convertSeqWithMappingImpl[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                                              (i        : c.Expr[I[A]],
                                                               mappings : c.Expr[FieldMappings])
                                                              (implicit ITag: c.WeakTypeTag[I[A]],
                                                                        ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertSeq(c)(i, Some(mappings), None)
    
    /* --- */
    
    def xml[A, I[_] <: IterableOnce[_]] (i             : I[A],
                                                mappings : FieldMappings,
                                                fieldName     : String) : Elem = macro convertSeqWithAllImpl[A, I]
    def convertSeqWithAllImpl[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                                          (i         : c.Expr[I[A]],
                                                           mappings  : c.Expr[FieldMappings],
                                                           fieldName : c.Expr[String])
                                                          (implicit ITag: c.WeakTypeTag[I[A]],
                                                                    ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertSeq(c)(i, Some(mappings), Some(fieldName))
    
    /* --- */
    
    def convertSeq[A, I[_] <: IterableOnce[_]] (c : blackbox.Context)
                                               (i         : c.Expr[I[A]],
                                                mappings  : Option[c.Expr[FieldMappings]],
                                                fieldName : Option[c.Expr[String]])
                                               (implicit ITag : c.WeakTypeTag[I[A]],
                                                         ATag : c.WeakTypeTag[A]): c.Expr[Elem] = { import c.universe._
//        println("iterable: " + ATag.tpe.typeSymbol.fullName)
        val lType = ITag.tpe
        val fullTypeName = lType.typeSymbol.fullName
        if ( ATag.tpe <:< ITag.tpe )  /* prohibit StackOverflow at compile time */
            createRuntimeConversion(c)(i, mappings, fieldName)
            
        else {
            val typeAsExpr = c.Expr[String](q"""$fullTypeName + "[" + ${ATag.tpe.typeSymbol.fullName} + "]" """)
            c.Expr[Elem](q"""
                scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
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
    
    def xml[A] (a : A): Elem = macro convertImpl[A]
    def xml[A] (a : A, mappings : FieldMappings): Elem = macro convertWithMappingImpl[A]
    def xml[A] (a : A, fieldName : String): Elem = macro convertWithFieldImpl[A]
    def xml[A] (a : A, mappings : FieldMappings, fieldName : String) : Elem = macro convertWithAllImpl[A]
    
    def convertImpl[A] (c: blackbox.Context)
                       (a: c.Expr[A])
                       (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, None, None)
    
    def convertWithMappingImpl[A] (c : blackbox.Context)
                                  (a : c.Expr[A], mappings: c.Expr[FieldMappings])
                                  (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, Some(mappings), None)
    
    def convertWithFieldImpl[A] (c : blackbox.Context)
                                (a : c.Expr[A], fieldName: c.Expr[String])
                                (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, None, Some(fieldName))
    
    def convertWithAllImpl[A] (c : blackbox.Context)
                              (a         : c.Expr[A],
                               mappings  : c.Expr[FieldMappings],
                               fieldName : c.Expr[String])
                              (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] =
        convertClassToXML(c)(a, Some(mappings), Some(fieldName))
    
    
    def convertClassToXML[A] (c : blackbox.Context)
                             (a         : c.Expr[A],
                              mappings  : Option[c.Expr[FieldMappings]],
                              fieldName : Option[c.Expr[String]])
                             (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] = { import c.universe._
              
        val aType = ATag.tpe
        val fullTypeName = aType.typeSymbol.fullName
        val typeAsExpr = c.Expr[String](q"$fullTypeName")
        
        if (isSimple(c)(aType))
            c.Expr[Elem](q"""
                scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                               $typeAsExpr,
                               scala.xml.Null,
                               scala.xml.TopScope,
                               true,
                               Seq(): _*) %
                scala.xml.Attribute("value", scala.xml.Text($a.toString()), scala.xml.Null)
            """)

            //todo array inside this method, instead of overloading
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
            createRuntimeConversion(c)(a, mappings, fieldName)
        
        else {
            c.Expr[Elem](
                aType.decls
                     .collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
                     .get
                     .paramLists
                     .head
                     .foldLeft(q"""scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
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
    
    def createRuntimeConversion[A] (c : blackbox.Context)
                                   (a         : c.Expr[A],
                                    mappings  : Option[c.Expr[FieldMappings]],
                                    fieldName : Option[c.Expr[String]])
                                   (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] = { import c.universe._
        c.Expr[Elem](q"""indv.jstengel.ezxml.extension.rt.RTConverter.convertToXML(
            $a,
            ${mappings.getOrElse(c.Expr[FieldMappings](q"indv.jstengel.ezxml.extension.mapping.FieldMappings()"))},
            ${fieldName.getOrElse(c.Expr[String](q"null"))}
        )""")
    }
    
}
