package jstengel.ezxml.extension.ct


import jstengel.ezxml.extension.XmlClassTrait

import scala.reflect.macros.blackbox


private[ct] object CompileTimeReflectHelper {
    
    /**
     * a type is considered simple (in this library at least), when it is a sub type of AnyVal, Number, or String
     * and a value of that type can therefor easily be extracted from a String
     * (mirrors jstengel.ezxml.extension.rt.RuntimeReflectHelper.isSimple)
     * @param c context, to access types and symbols, during compile time
     * @param t the type that will be checked
     * @return true, if the type is a sub type of AnyVal, Number, or String. false if that is not the case
     *         (and java.lang.boolean, since that is not inheriting from number for some reason)
     */
    def isSimple (c : blackbox.Context)(t : c.universe.Type) : Boolean =
        t <:< c.weakTypeOf[AnyVal] || t <:< c.weakTypeOf[String] || t <:< c.weakTypeOf[Number]
    
    /**
     * to determine, if we have access to private fields or not, this class checks where a macro was called from
     * @param c context, to access types and symbols, during compile time
     * @param tpe the type of the supposed enclosing class
     * @return true, if the macro is called to convert the enclosing class
     */
    def isMacroCalledFromEnclosingClass (c : blackbox.Context)(tpe : c.Type): Boolean =
        try {
            c.reifyEnclosingRuntimeClass.tpe.toString.contains(tpe.typeSymbol.fullName)
        } catch {
            case _ : NullPointerException => false
        }
    
    /**
     *
     * @param c context, to access types and symbols, during compile time
     * @param tpe the type for which the type parameters will be extracted
     * @return All TypeParameters as a List corresponding to tpe
     */
    def getTypeParams (c : blackbox.Context)(tpe: c.universe.Type): List[c.universe.Type] = tpe match {
        case c.universe.TypeRef(_, _, args) => args.asInstanceOf[List[c.universe.Type]]
        case _ => List()
    }
    
    /**
     * Checks if a constructor of a given type exists or not
     * @param c context, to access types and symbols, during compile time
     * @param tpe the type that will be checked
     * @return true, if there is no constructor , false if it exists
     */
    def isConstructorMissing(c : blackbox.Context)(tpe: c.Type): Boolean =
        tpe.members
           .collectFirst{ case m : c.universe.MethodSymbol if m.isPrimaryConstructor => m }
           .isEmpty
    
    /**
     * Checks if the given Type is created as an Iterable only and doesn't have a class constructor otherwise
     * @param c context, to access types and symbols, during compile time
     * @param tpe the type we will check
     * @param isCalledFromAnnotation boolean to see, if the macro was called from within the class that will be
     *                               operated upon
     * @return true, if tpe is only constructed as an iterable
     */
    def isConstructedThroughIterable(c : blackbox.Context)(tpe: c.Type, isCalledFromAnnotation: Boolean): Boolean =
        tpe.typeSymbol.fullName.startsWith("scala.collection") || (
            tpe <:< c.weakTypeOf[IterableOnce[_]] &&
            !(tpe <:< c.weakTypeOf[Product]) &&
            !isCalledFromAnnotation &&
            (isConstructorMissing(c)(tpe) || tpe.typeSymbol.isAbstract) )
    
    
    /**
     * because the types of the constructor params are likely to be generic, a map has to be applied to get the
     * correct type that can actually be loaded
     * This Function also retrieves all the other necessary information about this field
     * @param c                          context, to access types and symbols, during compile time
     * @param field                      the Symbol of the field from which all the information will be extracted
     * @param typeMap                    A Map that converts between the given tparams and the generic tparams from
     *                                   the constructor
     * @param createStringRepresentation a Function that will give the most accurate string representation containing
     *                                   all the information about the resulting type
     * @return a tuple5, that contains all the necessary information for the given field, in the following Order:
     *         - The name of the field
     *         - the actual type of the field
     *         - a correct string representation of that type
     *         - a boolean that is true, if the original field type was a vararg
     *         - another boolean, that is true if the field was annotated with @RuntimeXML
     */
    def getFieldInfo (c : blackbox.Context)
                     (field                      : c.Symbol,
                      typeMap                    : Map[c.Type, c.Type],
                      createStringRepresentation : c.Type => String): (String, c.Type, String, Boolean, Boolean) = {
        val fieldName                = field.name.encodedName.toString
        val fieldTypeSig             = field.typeSignature
        val tempFieldType            = typeMap.getOrElse(fieldTypeSig, fieldTypeSig)
        val tempFieldTypeAsString    = createStringRepresentation(tempFieldType)
        val isRepeated               = tempFieldTypeAsString.startsWith("scala.<repeated>")
        val shouldBeEncodedAtRuntime = field.annotations.exists(_.toString.contains("RuntimeXML"))
        if ( isRepeated ) {
            val repeatedType       = getTypeParams(c)(tempFieldType).head
            val actualRepeatedType = typeMap.getOrElse(repeatedType, repeatedType)
            /* create new Seq-type for the repeated field */
            val fieldTypeAsSeq     = c.typeOf[Seq[Nothing]] match {
                case c.universe.TypeRef(t, s, _) =>
                    c.internal.typeRef(t.asInstanceOf[c.universe.Type],
                                       s.asInstanceOf[c.universe.Symbol],
                                       List(actualRepeatedType))
            }
            (
                fieldName,
                fieldTypeAsSeq,
                s"scala.collection.immutable.Seq[$actualRepeatedType]",
                isRepeated,
                shouldBeEncodedAtRuntime
            )
        } else
            (fieldName, tempFieldType, tempFieldTypeAsString, isRepeated, shouldBeEncodedAtRuntime)
    }
    
    /**
     * @param loadedType the type that is checked if it is an object or not
     * @param companionSig the symbol corresponding to the companion of loadedType
     *                     (this could be extracted from loaded type, but since this is already done at call site,
     *                     it is passed as a parameter instead)
     * @return true if the type corresponds to an object, false if not
     */
    @inline def isObject (c : blackbox.Context)(loadedType : c.Type, companionSig : c.Type): Boolean = {
        companionSig.members.nonEmpty && loadedType.members.isEmpty
    }
    
    /**
     * This function loads the constructor of givenType and then maps the type parameters of the constructor to the
     * type parameters of givenType.
     * @param c context, to access types and symbols, during compile time
     * @param givenType the type for which the constructor will be loaded
     * @param typeParams the type params that were given to the macro.
     *                   These will be more specific than the type parameters of the constructor
     * @return a [[Tuple3]], where the first element is the constructor, the second is the TypeMap and the third is
     *         a List of Types, representing the type parameters of givenType
     */
    def getConstructorWithTypeMap(c: blackbox.Context)
                                 (givenType  : c.Type,
                                  typeParams : scala.List[c.Type]) : (c.universe.MethodSymbol,
                                                                     Map[c.universe.Type, c.Type],
                                                                     List[c.Type]) =  {
        val constructor     = givenType.decls
                                       .collectFirst{ case m : c.universe.MethodSymbol if m.isPrimaryConstructor => m }
                                       .get
        val constrTParams   = getTypeParams(c)(constructor.returnType)
        val classTypeParams = if ( typeParams.exists(_.toString.contains("<notype>")) )
                                  constrTParams
                              else
                                  typeParams
        val typeMap         = constrTParams.zip(classTypeParams).toMap
        (constructor, typeMap, classTypeParams)
    }
    
    
    
    def extendsXmlClassTrait (c : blackbox.Context)(givenType : c.Type): Boolean = {
        givenType.typeSymbol.asClass.baseClasses.contains(c.typeOf[XmlClassTrait])
    }
    
}
