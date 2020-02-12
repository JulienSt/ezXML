package jstengel.ezxml.extension.ct

import jstengel.ezxml.extension.XmlClassTrait
import jstengel.ezxml.extension.ct.CompileTimeReflectHelper._
import jstengel.ezxml.extension.mapping.FieldMapping.FieldMappings

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox
import scala.xml.Elem



//noinspection DuplicatedCode
// https://stackoverflow.com/questions/18450203/retrieve-the-name-of-the-value-a-scala-macro-invocation-will-be-assigned-to
object CtEncoder {
    
    /**
     * creates a conversion from a to an [[Elem]]
     * @param a the value that will be converted to an [[Elem]]
     * @tparam A the type that will be encoded
     * @return
     */
    def xml[A](a: A): Elem = macro xmlImpl[A]
    
    /**
     * implementation of [[xml]]
     * @param c context, to access types and symbols, during compile time
     *          (this is automatically added by calling the macro)
     * @param a the value that will be converted to an [[Elem]]
     * @param ATag the WeakTypeTag of [[A]], this is automatically filled in by the compiler
     * @tparam A the type that will be encoded
     * @return the expression that holds an [[Elem]] representation of a
     */
    def xmlImpl[A](c: blackbox.Context)(a: c.Expr[A])(implicit ATag : c.WeakTypeTag[A]) : c.Expr[Elem] = {
        import c.universe.Quasiquote
        c.Expr[Elem](q"jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[${ATag.tpe}]($a)")
    }
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @tparam A the type that the resulting function will be able to encode
     * @return a function that converts a value of type [[A]] to an [[Elem]]
     */
    def xmlMacro[A]: A => Elem = macro convertImpl[A]
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param mappings a possible [[FieldMappings]], where cross connections to other fields are cached
     * @tparam A the type that the resulting function will be able to encode
     * @return a function that converts a value of type [[A]] to an [[Elem]]
     */
    def xmlMacro[A] (mappings : FieldMappings): A => Elem = macro convertWithMappingImpl[A]
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param fieldName this represents a possible field the underlying Type [[A]] corresponds to
     * @tparam A the type that the resulting function will be able to encode
     * @return a function that converts a value of type [[A]] to an [[Elem]]
     */
    def xmlMacroAsField[A] (fieldName : String): A => Elem = macro convertWithFieldImpl[A]
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param mappings  a possible [[FieldMappings]], where cross connections to other fields are cached
     * @param fieldName this represents a possible field the underlying Type [[A]] corresponds to
     * @tparam A the type that the resulting function will be able to encode
     * @return a function that converts a value of type [[A]] to an [[Elem]]
     */
    def xmlMacro[A] (mappings : FieldMappings, fieldName : String) : A => Elem = macro convertWithAllImpl[A]
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param c    context, to access types and symbols, during compile time
     *             (this is automatically added by calling the macro)
     * @param ATag the WeakTypeTag of [[A]], this is automatically filled in by the compiler
     * @tparam A the type that the resulting function will be able to encode
     * @return an Expression of a function that can encode an object of type [[A]] in an [[Elem]]
     */
    def convertImpl[A] (c: blackbox.Context)(implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] =
        convertClassToXML(c)(None, None)
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param c        context, to access types and symbols, during compile time
     *                 (this is automatically added by calling the macro)
     * @param mappings a possible [[FieldMappings]], where cross connections to other fields are cached
     * @param ATag     the WeakTypeTag of [[A]], this is automatically filled in by the compiler
     * @tparam A the type that the resulting function will be able to encode
     * @return an Expression of a function that can encode an object of type [[A]] in an [[Elem]]
     */
    def convertWithMappingImpl[A] (c : blackbox.Context)
                                  (mappings: c.Expr[FieldMappings])
                                  (implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] =
        convertClassToXML(c)(Some(mappings), None)
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param c         context, to access types and symbols, during compile time
     *                  (this is automatically added by calling the macro)
     * @param fieldName this represents a possible field the underlying Type [[A]] corresponds to
     * @param ATag      the WeakTypeTag of [[A]], this is automatically filled in by the compiler
     * @tparam A the type that the resulting function will be able to encode
     * @return an Expression of a function that can encode an object of type [[A]] in an [[Elem]]
     */
    def convertWithFieldImpl[A] (c : blackbox.Context)
                                (fieldName: c.Expr[String])
                                (implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] =
        convertClassToXML(c)(None, Some(fieldName))
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param c         context, to access types and symbols, during compile time
     *                  (this is automatically added by calling the macro)
     * @param mappings  a possible [[FieldMappings]], where cross connections to other fields are cached
     * @param fieldName this represents a possible field the underlying Type [[A]] corresponds to
     * @param ATag      the WeakTypeTag of [[A]], this is automatically filled in by the compiler
     * @tparam A the type that the resulting function will be able to encode
     * @return an Expression of a function that can encode an object of type [[A]] in an [[Elem]]
     */
    def convertWithAllImpl[A] (c : blackbox.Context)
                              (mappings  : c.Expr[FieldMappings],
                               fieldName : c.Expr[String])
                              (implicit ATag : c.WeakTypeTag[A]) : c.Expr[A => Elem] =
        convertClassToXML(c)(Some(mappings), Some(fieldName))
    
    
    /**
     * This macro creates a function that converts the given type [[A]] to an [[Elem]]
     * @param c         context, to access types and symbols, during compile time
     *                  (this is automatically added by calling the macro)
     * @param mappings  a possible [[FieldMappings]], where cross connections to other fields are cached
     * @param fieldName this will be the prefix to the root node
     *                  this makes the children identifiable, when they are decoded and loaded into the correct field
     * @param ATag      the WeakTypeTag of [[A]], this is automatically filled in by the compiler
     * @tparam A the type that the resulting function will be able to encode
     * @return an Expression of a function that can encode an object of type [[A]] in an [[Elem]]
     */
    def convertClassToXML[A] (c : blackbox.Context)
                             (mappings  : Option[c.Expr[FieldMappings]],
                              fieldName : Option[c.Expr[String]])
                             (implicit ATag : c.WeakTypeTag[A]) : c.Expr[A => Elem] = {
        
        import c.universe.{Quasiquote, TermName, Type, typeOf}
        
        /**
         * creates a string representation for a given type, such that it can be loaded through RtDecoder.load,
         * CtDecoder.obj, or getTypeFromString
         * Sadly, this needs to be a nested function and can not be generalize, due to compiler problems
         *
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
            
        else if (aType <:< typeOf[Array[_]]) // todo include mapping
            c.Expr[A => Elem](q""" (objectToBeEncoded: $aType) =>
                scala.xml.Elem(${fieldName.getOrElse(c.Expr[String](q"null"))},
                               $fullTypeName,
                               scala.xml.Null,
                               scala.xml.TopScope,
                               false,
                               objectToBeEncoded.map{e =>
                                   jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[..$typeParams](e)}.toIndexedSeq: _*)
                """)
            
        else if (isConstructedThroughIterable(c)(aType, isCalledFromEnclosingClass))
            encodeIterable(c)(mappings, fieldName, aType, typeParams, fullTypeName)
            
        else if (aType.typeSymbol.isAbstract || mappings.nonEmpty) /* since mapping is runtime dependent */
            createRuntimeConversion(c)(aType, mappings, fieldName)

        else {
            val (constructor, typeMap, classTypeParams) = getConstructorWithTypeMap(c)(aType, typeParams)
            val fullTypeName                            = createStringRepresentation(aType)(classTypeParams)
            encodeClass(c)(constructor,
                         fullTypeName,
                         typeMap,
                         fieldName,
                         createStringRepresentation(_)(),
                         aType,
                         isCalledFromEnclosingClass)
        }
    }
    
    /**
     * creates a function that encodes a value of type [[A]] in an [[Elem]], where [[A]] is a class
     * @param c                          context, to access types and symbols, during compile time
     * @param constructor                constructor of the class that will be encoded
     * @param fullTypeName               the complete type name of [[A]], including type params and path
     * @param typeMap                    A Map that converts between the given tparams and the generic tparams from
     *                                   the constructor
     * @param fieldName                  this represents a possible field the underlying iterable corresponds to
     * @param createStringRepresentation a Function that will give the most accurate string representation containing
     *                                   all the information about the resulting type
     * @param aType                      the type of [[A]] as c.Type
     * @param isCalledFromEnclosingClass boolean to tell from where the macro was originally called from
     * @tparam A the type of Class the resulting function will be able to encode
     * @return a function that can encode a iterable of type [[A]] in an [[Elem]]
     */
    private def encodeClass[A] (c : blackbox.Context)
                               (constructor                : c.universe.MethodSymbol,
                                fullTypeName               : String,
                                typeMap                    : Map[c.Type, c.Type],
                                fieldName                  : Option[c.Expr[String]],
                                createStringRepresentation : c.Type =>String,
                                aType                      : c.Type,
                                isCalledFromEnclosingClass : Boolean) = {
        import c.universe.{Quasiquote, Symbol, TermName, Tree, TypeName}
        c.Expr[A => Elem]({
            
            val elemAsString = constructor
                .paramLists
                .flatten
                .foldLeft(q"""scala.xml.Elem(${ fieldName.getOrElse(c.Expr[String](q"null")) },
                                             $fullTypeName,
                                             scala.xml.Null,
                                             scala.xml.TopScope,
                                             true,
                                             Seq(): _*)"""){ case (quote : Tree, field : Symbol) =>
                    val fName                             = TermName(field.name.decodedName.toString)
                    val (fieldType, fieldTypeAsString, _) =
                        getActualFieldType(c)(field.typeSignature, typeMap, createStringRepresentation(_))
                    
                    // todo annotations
                    // todo check which annotations can be used together
                    // pattern matching through all annotations, since they can be combined
//                        val annotations = field.annotations.flatMap(_.tree match {
//                            case q"new $tpname($param)" if isValid(tpname.toString()) => Some
//                            (tpname, Some(param))
//                            case q"new $tpname"         if isValid(tpname.toString()) => Some
//                            (tpname, None)
//                            case _ => None
//                        })
//                        println("---------------------- Annotations ----------------------")
//                        annotations.foreach(println)
//                        println("^^^^^^^^^^^^^^^^^^^^^^ Annotations ^^^^^^^^^^^^^^^^^^^^^^")
                    
                    val fieldCall = if ( isCalledFromEnclosingClass )
                                        q"$fName" // todo check if field is annotated with substitution
                                    else
                                        q"${ TermName("objectToBeEncoded") }.$fName"
                    
                    if ( isSimple(c)(fieldType) )
                        q"""$quote % scala.xml.Attribute($fieldTypeAsString,
                                                               ${ fName.toString },
                                                               scala.xml.Text($fieldCall.toString),
                                                               scala.xml.Null)"""
                    else if ( fieldType.typeSymbol.isAbstract && fieldType.baseClasses.length <= 1 )
                             q"""val scala.xml.Elem(prefix, label, attribs, scope, child @ _*) = $quote
                                scala.xml.Elem(prefix, label, attribs, scope, false, child ++
                                jstengel.ezxml.extension.ct.CtEncoder.xmlMacroAsField[${
                                 TypeName(fieldTypeAsString)
                             }](${ fName.toString })($fieldCall): _*)"""
                    else
                        q"""val scala.xml.Elem(prefix, label, attribs, scope, child @ _*) = $quote
                                scala.xml.Elem(prefix, label, attribs, scope, false, child ++
                                jstengel.ezxml.extension.ct.CtEncoder.xmlMacroAsField[$fieldType](${
                            fName.toString
                        })($fieldCall): _*)"""
                }
            q"(objectToBeEncoded: $aType) => $elemAsString"
        })
    }
    
    /**
     * creates a function that encodes a value of type [[A]] in an [[Elem]], where [[A]] is an Iterable
     * @param c            context, to access types and symbols, during compile time
     * @param mappings     a possible [[FieldMappings]], where cross connections to other fields are cached
     * @param fieldName    this represents a possible field the underlying iterable corresponds to
     * @param aType        the type of [[A]] as c.Type
     * @param typeParams   the type params corresponding to the iterable
     * @param fullTypeName the complete type name of [[A]], including type params and path
     * @tparam A the type of iterable the resulting function will be able to encode
     * @return a function that can encode a iterable of type [[A]] in an [[Elem]]
     */
    @inline private def encodeIterable[A] (c : blackbox.Context)
                                          (mappings     : Option[c.Expr[FieldMappings]],
                                           fieldName    : Option[c.Expr[String]],
                                           aType        : c.Type,
                                           typeParams   : List[c.Type],
                                           fullTypeName : String): c.Expr[A => Elem] = {
        import c.universe.Quasiquote
        /* prohibit StackOverflow at compile time */
        if ( typeParams.length == 1 && aType <:< typeParams.head )
            createRuntimeConversion(c)(aType, mappings, fieldName)
        else
            c.Expr[A => Elem]({
                val macroLambda = (mappings, typeParams.length) match {
                    case (Some(m), 1) => q"""jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[..$typeParams]($m)"""
                    case (None   , 1) => q"""jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[..$typeParams]"""
                    case (Some(m), _) => q"""jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[(..$typeParams)]($m)"""
                    case (None   , _) => q"""jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[(..$typeParams)]"""
                }
                q"""(objectToBeEncoded: $aType) => scala.xml.Elem(
                    ${ fieldName.getOrElse(c.Expr[String](q"null")) },
                    $fullTypeName,
                    scala.xml.Null,
                    scala.xml.TopScope,
                    false,
                    objectToBeEncoded.iterator.map(e => $macroLambda(e)).toSeq: _*)"""
            })
    }
    
    /**
     * creates a function call to RtEncoder.convertToXML
     * @param c         context, to access types and symbols, during compile time
     * @param mappings  an optional expression of [[FieldMappings]] containing all the information
     * @param fieldName this represents a possible field the underlying simple type corresponds to
     * @param ATag      the type of [[A]] as c.Type
     * @tparam A the specific type the resulting function will be able to encode
     * @return an Expression containing a function call to RtEncoder.convertToXML
     */
    def createRuntimeConversion[A] (c : blackbox.Context)
                                   (tpe : c.Type,
                                    mappings  : Option[c.Expr[FieldMappings]],
                                    fieldName : Option[c.Expr[String]])
                                   (implicit ATag: c.WeakTypeTag[A]): c.Expr[A => Elem] = { import c.universe._
        c.Expr[A => Elem](q""" (objectToBeEncoded: $tpe) => jstengel.ezxml.extension.rt.RtEncoder.convertToXML(
            objectToBeEncoded,
            ${mappings.getOrElse(c.Expr[FieldMappings](q"Seq()"))},
            ${fieldName.getOrElse(c.Expr[String](q"null"))}
        )""")
    }
    
}
