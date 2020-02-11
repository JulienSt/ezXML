package indv.jstengel.ezxml.extension.ct


import scala.reflect.macros.blackbox


private[ct] object CompileTimeReflectHelper {
    
    /**
     * a type is considered simple (in this library at least), when it is a sub type of AnyVal, Number, or String
     * and a value of that type can therefor easily be extracted from a String
     * (mirrors indv.jstengel.ezxml.extension.rt.RuntimeReflectHelper.isSimple)
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
    def isMacroCallingEnclosingClass (c : blackbox.Context)(tpe : c.Type): Boolean =
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
        tpe <:< c.weakTypeOf[IterableOnce[_]] &&
        !isCalledFromAnnotation &&
        (isConstructorMissing(c)(tpe) || tpe.typeSymbol.isAbstract)
    
    
    /**
     * because the types of the constructor params are likely to be generic, a map has to be applied to get the
     * correct type, that can actually be loaded
     * @param c context, to access types and symbols, during compile time
     * @param fieldTypeSig the most generic type signatures of the field
     * @param typeMap A Map that converts between the given tparams and the generic tparams from the constructor
     * @param createStringRepresentation a Function that will give the most accurate string representation containing
     *                                   all the information about the resulting type
     * @return a tuple3, that contains the actual type, a correct string representation of that type and a boolean,
     *         that is true, if the original field type was a vararg
     */
    def getActualFieldType(c : blackbox.Context)
                          (fieldTypeSig: c.Type,
                           typeMap: Map[c.Type, c.Type],
                           createStringRepresentation: c.Type => String): (c.Type, String, Boolean) = {
        val tempFieldType = typeMap.getOrElse(fieldTypeSig, fieldTypeSig)
        val tempFieldTypeAsString = createStringRepresentation(tempFieldType)
        val isRepeated = tempFieldTypeAsString.startsWith("scala.<repeated>")
        if (isRepeated) {
            val repeatedType = getTypeParams(c)(tempFieldType).head
            val actualRepeatedType = typeMap.getOrElse(repeatedType, repeatedType)
            val fieldTypeAsSeq = c.typeOf[Seq[Nothing]] match {
                case c.universe.TypeRef(t, s, _) =>
                    c.internal.typeRef(t.asInstanceOf[c.universe.Type],
                                       s.asInstanceOf[c.universe.Symbol],
                                       List(actualRepeatedType))
            }
            (fieldTypeAsSeq ,s"scala.collection.immutable.Seq[$actualRepeatedType]", isRepeated)
        } else {
            (tempFieldType, tempFieldTypeAsString, isRepeated)
        }
    }
    
    /**
     * @param loadedType the type that is checked if it is an object or not
     * @param loadedSymbol the symbol corresponding to loadedType
     *                     (this could be extracted from loaded type, but since this is already done at call site,
     *                     it is passed as a parameter instead)
     * @return true if the type corresponds to an object, false if not
     */
    @inline def isObject (c : blackbox.Context)(loadedType : c.Type, companionSig : c.Type): Boolean = {
        companionSig.members.nonEmpty && loadedType.members.isEmpty
    }
    
}
