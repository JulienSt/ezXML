package indv.jstengel.ezxml.extension.ct


import scala.reflect.macros.blackbox


object CompileTimeReflectHelper {
    
    /**
     * a type is considered simple (in this library at least), when it is a sub type of AnyVal, Number, or String
     * and a value of that type can therefor easily be extracted from a String
     * (mirrors indv.jstengel.ezxml.extension.rt.RuntimeReflectHelper.isSimple)
     * @param c context, to access types and symbols, during compile time
     * @param t the type that will be checked
     * @return true, if the type is a sub type of AnyVal, Number, or String. false if that is not the case
     *         (and java.lang.boolean, since that is not inheriting from number for some reason)
     */
    private[ct] def isSimple (c : blackbox.Context)(t : c.universe.Type) =
        t <:< c.weakTypeOf[AnyVal] || t <:< c.weakTypeOf[String] || t <:< c.weakTypeOf[Number]
    
    /**
     * to determine, if we have access to private fields or not, this class checks where a macro was called from
     * @param c context, to access types and symbols, during compile time
     * @param fullTypeName the name of the enclosing class
     * @return true, if the macro is called to convert the enclosing class
     */
    private[ct] def isMacroCallingEnclosingClass (c : blackbox.Context, fullTypeName : String): Boolean =
        try {
            c.reifyEnclosingRuntimeClass.tpe.toString.contains(fullTypeName)
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
     *
     * @param c
     * @param tpe
     * @param isCalledFromAnnotation
     * @return
     */
    def isConstructedThroughIterable(c : blackbox.Context)(tpe: c.Type, isCalledFromAnnotation: Boolean): Boolean =
        tpe <:< c.weakTypeOf[IterableOnce[_]] &&
        !isCalledFromAnnotation &&
        (isConstructorMissing(c)(tpe) || tpe.typeSymbol.isAbstract)
    
}
