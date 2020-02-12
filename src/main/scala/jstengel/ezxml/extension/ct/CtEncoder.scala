package indv.jstengel.ezxml.extension.ct


import indv.jstengel.ezxml.extension.XmlClassTrait
import indv.jstengel.ezxml.extension.ct.CompileTimeReflectHelper._
import indv.jstengel.ezxml.extension.mapping.FieldMapping.FieldMappings

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox
import scala.xml.Elem



// https://stackoverflow.com/questions/18450203/retrieve-the-name-of-the-value-a-scala-macro-invocation-will-be-assigned-to
object CtEncoder {
    
    def xml[A](a: A): Elem = macro xmlImpl[A]
    
    def xmlImpl[A](c: blackbox.Context)(a: c.Expr[A])(implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] = {
        import c.universe._
        c.Expr[Elem](q"indv.jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[${ATag.tpe}]($a)")
    }
    
    /**
     *
     * @tparam A
     * @return
     */
    def xmlMacro[A]: A => Elem = macro convertImpl[A]
    
    /**
     *
     * @param mappings
     * @tparam A
     * @return
     */
    def xmlMacro[A] (mappings : FieldMappings): A => Elem = macro convertWithMappingImpl[A]
    
    /**
     *
     * @param fieldName
     * @tparam A
     * @return
     */
    def xmlMacroAsField[A] (fieldName : String): A => Elem = macro convertWithFieldImpl[A]
    
    /**
     *
     * @param mappings
     * @param fieldName
     * @tparam A
     * @return
     */
    def xmlMacro[A] (mappings : FieldMappings, fieldName : String) : A => Elem = macro convertWithAllImpl[A]
    
    /**
     *
     * @param c
     * @param ATag
     * @tparam A
     * @return
     */
    def convertImpl[A] (c: blackbox.Context)
                       (implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] =
        convertClassToXML(c)(None, None)
    
    /**
     *
     * @param c
     * @param mappings
     * @param ATag
     * @tparam A
     * @return
     */
    def convertWithMappingImpl[A] (c : blackbox.Context)
                                  (mappings: c.Expr[FieldMappings])
                                  (implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] =
        convertClassToXML(c)(Some(mappings), None)
    
    /**
     *
     * @param c
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def convertWithFieldImpl[A] (c : blackbox.Context)
                                (fieldName: c.Expr[String])
                                (implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] =
        convertClassToXML(c)(None, Some(fieldName))
    
    /**
     *
     * @param c
     * @param mappings
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def convertWithAllImpl[A] (c : blackbox.Context)
                              (mappings  : c.Expr[FieldMappings],
                               fieldName : c.Expr[String])
                              (implicit ATag : c.WeakTypeTag[A]) : c.Expr[A => Elem] =
        convertClassToXML(c)(Some(mappings), Some(fieldName))
    
    
    /**
     *
     * @param c
     * @param mappings
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def convertClassToXML[A] (c : blackbox.Context)
                             (mappings  : Option[c.Expr[FieldMappings]],
                              fieldName : Option[c.Expr[String]])
                             (implicit ATag : c.WeakTypeTag[A]) : c.Expr[A => Elem] = { import c.universe._
        
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
    
        val aType                      = ATag.tpe
        val typeParams                 = getTypeParams(c)(aType)
        val fullTypeName               = createStringRepresentation(aType)(typeParams)
        val isCalledFromEnclosingClass = isMacroCalledFromEnclosingClass(c)(aType)
        
        
        if (!isCalledFromEnclosingClass && aType <:< typeOf[XmlClassTrait])
            c.Expr[A => Elem](q"""(objectToBeEncoded: $aType) => objectToBeEncoded.${TermName("saveAsXml")}""")

        else if (isSimple(c)(aType))
            c.Expr[A => Elem](q""" (objectToBeEncoded: $aType) =>
                scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                               $fullTypeName,
                               scala.xml.Null,
                               scala.xml.TopScope,
                               true,
                               Seq(): _*) %
                scala.xml.Attribute("value", scala.xml.Text(objectToBeEncoded.toString()), scala.xml.Null)
            """)
            
        else if (aType <:< typeOf[Array[_]])
            c.Expr[A => Elem](q""" (objectToBeEncoded: $aType) =>
                scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                               $fullTypeName,
                               scala.xml.Null,
                               scala.xml.TopScope,
                               false,
                               objectToBeEncoded.map(e => indv.jstengel
                                                              .ezxml
                                                              .extension
                                                              .ct
                                                              .CtEncoder
                                                              .xmlMacro[..$typeParams](e)).toIndexedSeq: _*)
                """) // todo include mapping

        else if (isConstructedThroughIterable(c)(aType, isCalledFromEnclosingClass)) {
            if ( typeParams.length == 1 && ATag.tpe <:< typeParams.head )  /* prohibit StackOverflow at compile time */
                createRuntimeConversion(c)(aType, mappings, fieldName)
            else if (typeParams.length > 1)
                c.Expr[A => Elem](q""" (objectToBeEncoded: $aType) =>
                    scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                        $fullTypeName,
                        scala.xml.Null,
                        scala.xml.TopScope,
                        false,
                        objectToBeEncoded.iterator
                                         .map(e => indv.jstengel
                                                       .ezxml
                                                       .extension
                                                       .ct
                                                       .CtEncoder
                                                       .xmlMacro[(..$typeParams)](e)).toSeq: _*)
                """)
            else
                c.Expr[A => Elem](q""" (objectToBeEncoded: $aType) =>
                    scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                        $fullTypeName,
                        scala.xml.Null,
                        scala.xml.TopScope,
                        false,
                        objectToBeEncoded.iterator
                                         .map(e => indv.jstengel
                                                       .ezxml
                                                       .extension
                                                       .ct
                                                       .CtEncoder
                                                       .xmlMacro[..$typeParams](e)).toSeq: _*)
                """)
            // todo todo include mapping
        }
        
        else if (aType.typeSymbol.isAbstract || mappings.nonEmpty) // todo check if an empty mapping is a reasonable test
            createRuntimeConversion(c)(aType, mappings, fieldName)
        
        else
            c.Expr[A => Elem]({
                val (constructor, typeMap, classTypeParams) = getConstructorWithTypeMap(c)(aType, typeParams)
                val fullTypeName            = createStringRepresentation(aType)(classTypeParams)
                val elemAsString            = constructor
                    .paramLists
                    .flatten
                    .foldLeft(q"""scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                                                 $fullTypeName,
                                                 scala.xml.Null,
                                                 scala.xml.TopScope,
                                                 true,
                                                 Seq(): _*)""") { case (quote: Tree, field : Symbol) =>
                        val fName     = TermName(field.name.decodedName.toString)
                        val (fieldType, fieldTypeAsString, _) =
                            getActualFieldType(c)(field.typeSignature, typeMap, createStringRepresentation(_)())
                        
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
                            if(isCalledFromEnclosingClass)
                                q"$fName" // todo check if field is annotated with substitution
                            else
                                q"${TermName("objectToBeEncoded")}.$fName"
                        
                        // todo if the type already has a conversion method, call that instead
//                        if (tempFieldType.typeSymbol.isAbstract && fieldSig.baseClasses.length == 1)
//                            throw new TypeNotPresentException(
//                                tempFieldType.toString,
//                                new Exception("Due to some complicated issues during compile time," +
//                                              "your class can not be converted to xml with this macro." +
//                                              "\nPlease use the runtime method instead. " +
//                                              "As an alternative")
//                                )
//                        else if(tempFieldType.typeSymbol.isAbstract)
//                            q"""indv.jstengel.ezxml.extension.rt.RtEncoder.convertToXML(
//                                    $fieldCall,
//                                    ${ mappings.getOrElse(c.Expr[FieldMappings](q"Seq()")) },
//                                    ${ fName.toString })
//                            """
//                        println(fieldCall)
                        
                        if (isSimple(c)(fieldType))
                            q"""$quote % scala.xml.Attribute($fieldTypeAsString,
                                                               ${fName.toString},
                                                               scala.xml.Text($fieldCall.toString),
                                                               scala.xml.Null)"""
                        else if (fieldType.typeSymbol.isAbstract && fieldType.baseClasses.length <= 1)
                            q"""val scala.xml.Elem(prefix, label, attribs, scope, child @ _*) = $quote
                                scala.xml.Elem(prefix, label, attribs, scope, false, child ++
                                indv.jstengel.ezxml.extension.ct
                                .CtEncoder.xmlMacroAsField[${TypeName(fieldTypeAsString)}](${fName.toString})($fieldCall): _*)"""
                        else
                            q"""val scala.xml.Elem(prefix, label, attribs, scope, child @ _*) = $quote
                                scala.xml.Elem(prefix, label, attribs, scope, false, child ++
                                indv.jstengel.ezxml.extension.ct
                                .CtEncoder.xmlMacroAsField[$fieldType](${fName.toString})($fieldCall): _*)"""
                    }
                q"(objectToBeEncoded: $aType) => $elemAsString"
            })
    }
    
    
    /**
     *
     * @param c
     * @param mappings
     * @param fieldName
     * @param ATag
     * @tparam A
     * @return
     */
    def createRuntimeConversion[A] (c : blackbox.Context)
                                   (tpe : c.Type,
                                    mappings  : Option[c.Expr[FieldMappings]],
                                    fieldName : Option[c.Expr[String]])
                                   (implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] = { import c.universe._
        c.Expr[A => Elem](q""" (objectToBeEncoded: $tpe) => indv.jstengel.ezxml.extension.rt.RtEncoder.convertToXML(
            objectToBeEncoded,
            ${mappings.getOrElse(c.Expr[FieldMappings](q"Seq()"))},
            ${fieldName.getOrElse(c.Expr[String](q"null"))}
        )""")
    }
    
}
