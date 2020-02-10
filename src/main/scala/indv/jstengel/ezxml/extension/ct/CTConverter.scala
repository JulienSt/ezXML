package indv.jstengel.ezxml.extension.ct


import indv.jstengel.ezxml.extension.ct.CompileTimeReflectHelper.{getTypeParams, isConstructedThroughIterable, isMacroCallingEnclosingClass, isSimple}
import indv.jstengel.ezxml.extension.mapping.FieldMapping.FieldMappings

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox
import scala.xml.Elem



// https://stackoverflow.com/questions/18450203/retrieve-the-name-of-the-value-a-scala-macro-invocation-will-be-assigned-to
object CTConverter {
    
    /**
     *
     * @param a
     * @tparam A
     * @return
     */
    def xml[A] (a : A): Elem = macro convertImpl[A]
    
    def xmlAnnotation[A] (a : A): Elem = macro convertAnnotationImpl[A]
    
    /**
     *
     * @param a
     * @param mappings
     * @tparam A
     * @return
     */
    def xml[A] (a : A, mappings : FieldMappings): Elem = macro convertWithMappingImpl[A]
    
    /**
     *
     * @param a
     * @param fieldName
     * @tparam A
     * @return
     */
    def xml[A] (a : A, fieldName : String): Elem = macro convertWithFieldImpl[A]
    
    /**
     *
     * @param a
     * @param mappings
     * @param fieldName
     * @tparam A
     * @return
     */
    def xml[A] (a : A, mappings : FieldMappings, fieldName : String) : Elem = macro convertWithAllImpl[A]
    
    /**
     *
     * @param c
     * @param a
     * @param ATag
     * @tparam A
     * @return
     */
    def convertImpl[A] (c: blackbox.Context)
                       (a: c.Expr[A])
                       (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, None, None, false)
    /**
     *
     * @param c
     * @param a
     * @param ATag
     * @tparam A
     * @return
     */
    def convertAnnotationImpl[A] (c: blackbox.Context)
                       (a: c.Expr[A])
                       (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, None, None, isCalledFromAnnotation = true)
    
    /**
     *
     * @param c
     * @param a
     * @param mappings
     * @param ATag
     * @tparam A
     * @return
     */
    def convertWithMappingImpl[A] (c : blackbox.Context)
                                  (a : c.Expr[A], mappings: c.Expr[FieldMappings])
                                  (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, Some(mappings), None, false)
    
    /**
     *
     * @param c
     * @param a
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def convertWithFieldImpl[A] (c : blackbox.Context)
                                (a : c.Expr[A], fieldName: c.Expr[String])
                                (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] =
        convertClassToXML(c)(a, None, Some(fieldName), false)
    
    /**
     *
     * @param c
     * @param a
     * @param mappings
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def convertWithAllImpl[A] (c : blackbox.Context)
                              (a         : c.Expr[A],
                               mappings  : c.Expr[FieldMappings],
                               fieldName : c.Expr[String])
                              (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] =
        convertClassToXML(c)(a, Some(mappings), Some(fieldName), false)
    
    
    /**
     *
     * @param c
     * @param a
     * @param mappings
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def convertClassToXML[A] (c : blackbox.Context)
                             (a         : c.Expr[A],
                              mappings  : Option[c.Expr[FieldMappings]],
                              fieldName : Option[c.Expr[String]],
                              isCalledFromAnnotation: Boolean = false)
                             (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] = {
        
        import c.universe._
        
        /**
         * creates a string representation for a given type, such that it can be loaded through RTLoader.load,
         * CTLoader.obj, or getTypeFromString
         * Sadly, this needs to be a nested function and can not be generalize, due to compiler problems
         * @param t the type that will be converted to a string
         * @param typeParams the type params that will be included in the string representation of t
         * @return a String representation for type t in the for of t[typeParams]
         */
        def createStringRepresentation (t : Type)(typeParams : List[Type] = getTypeParams(c)(t)): String =
            if (typeParams.isEmpty)
                t.typeSymbol.fullName
            else
                s"${t.typeSymbol.fullName}[${typeParams.map(t => createStringRepresentation(t)()).mkString(",")}]"
    
        val aType : Type = ATag.tpe
        val typeParams   = getTypeParams(c)(aType)
        val fullTypeName = createStringRepresentation(aType)() // todo is missing typeParameter in string
        val typeAsExpr   = c.Expr[String](q"$fullTypeName")
        
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

        else if (aType <:< typeOf[Array[_]])
            c.Expr[Elem](q"""
                scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                               $fullTypeName,
                               scala.xml.Null,
                               scala.xml.TopScope,
                               false,
                               $a.map(e => indv.jstengel.ezxml.extension.ct.CTConverter.xml(e)).toIndexedSeq: _*)
                """) // todo include mapping

        else if (isConstructedThroughIterable(c)(aType, isCalledFromAnnotation))
            if ( typeParams.length == 1 && ATag.tpe <:< typeParams.head )  /* prohibit StackOverflow at compile time */
                createRuntimeConversion(c)(a, mappings, fieldName)
            else
                c.Expr[Elem](q"""
                    scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                                   $fullTypeName,
                                   scala.xml.Null,
                                   scala.xml.TopScope,
                                   false,
                                   $a.iterator.map(e => indv.jstengel.ezxml.extension.ct.CTConverter.xml(e)).toSeq: _*)
                """) // todo todo include mapping
            
        else if (aType.typeSymbol.isAbstract || mappings.nonEmpty) // todo check if an empty mapping is a reasonable test
            createRuntimeConversion(c)(a, mappings, fieldName)
        
        else {
            c.Expr[Elem]({
                val constructor = aType.decls
                                       .collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
                                       .get
                val typeMap = getTypeParams(c)(constructor.returnType).zip(getTypeParams(c)(aType)).toMap
                val isCallingEnclosingClass = isMacroCallingEnclosingClass(c)(aType)
                constructor
                    .paramLists
                    .head
                    .foldLeft(q"""scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                                                 $typeAsExpr,
                                                 scala.xml.Null,
                                                 scala.xml.TopScope,
                                                 true,
                                                 Seq(): _*)""") { case (quote, field : Symbol) =>
                        val fName     = TermName(field.name.decodedName.toString)
                        val fieldSig  = field.typeSignature
                        val fieldType = typeMap.getOrElse(fieldSig, fieldSig)
                        
                        // todo annotations
                        // todo check which annotations can be used together
                        //  pattern matching through all annotations, since they can be combined
//                        val annotations = field.annotations.flatMap(_.tree match {
//                            case q"new $tpname($param)" if isValid(tpname.toString()) => Some(tpname, Some(param))
//                            case q"new $tpname"         if isValid(tpname.toString()) => Some(tpname, None)
//                            case _ => None
//                        })
//                        println("---------------------- Annotations ----------------------")
//                        annotations.foreach(println)
//                        println("^^^^^^^^^^^^^^^^^^^^^^ Annotations ^^^^^^^^^^^^^^^^^^^^^^")
                        
                        // todo if fieldmapping is not empty, do a runtime conversion
//                         val substituteField = mappings.getOrElse(c.Expr[FieldMappings](
//                            q"indv.jstengel.ezxml.extension.mapping.FieldMappings()"
//                        ))
                        
                        val fieldCall =
                            if(isCallingEnclosingClass)
                                q"$fName" // todo check if field is annotated with substitution
                            else
                                q"$a.$fName"
                        
                        // todo if the type already has a conversion method, call that instead
//                        if (fieldType.typeSymbol.isAbstract && fieldSig.baseClasses.length == 1)
//                            throw new TypeNotPresentException(
//                                fieldType.toString,
//                                new Exception("Due to some complicated issues during compile time," +
//                                              "your class can not be converted to xml with this macro." +
//                                              "\nPlease use the runtime method instead. " +
//                                              "As an alternative")
//                                )
//                        else if(fieldType.typeSymbol.isAbstract)
//                            q"""indv.jstengel.ezxml.extension.rt.RTConverter.convertToXML(
//                                    $fieldCall,
//                                    ${ mappings.getOrElse(c.Expr[FieldMappings](q"Seq()")) },
//                                    ${ fName.toString })
//                            """
                        if (isSimple(c)(fieldType))
                            q"""$quote % scala.xml.Attribute(${ fieldType.typeSymbol.fullName.toString },
                                                             ${ fName.toString },
                                                             scala.xml.Text($fieldCall.toString),
                                                             scala.xml.Null)"""
                        else
                            q"""val scala.xml.Elem(prefix, label, attribs, scope, child @ _*) = $quote
                                scala.xml.Elem(prefix, label, attribs, scope, false, child ++
                                indv.jstengel.ezxml.extension.ct.CTConverter.xml($fieldCall,${fName.toString}): _*)"""
                }}
            )
        }
    }
    
    /**
     *
     * @param c
     * @param a
     * @param mappings
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def createRuntimeConversion[A] (c : blackbox.Context)
                                   (a         : c.Expr[A],
                                    mappings  : Option[c.Expr[FieldMappings]],
                                    fieldName : Option[c.Expr[String]])
                                   (implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem] = { import c.universe._
        c.Expr[Elem](q"""indv.jstengel.ezxml.extension.rt.RTConverter.convertToXML(
            $a,
            ${mappings.getOrElse(c.Expr[FieldMappings](q"Seq()"))},
            ${fieldName.getOrElse(c.Expr[String](q"null"))}
        )""")
    }
    
}
