package indv.jstengel.ezxml.extension.ct


import scala.reflect.macros.blackbox


object CompileTimeReflectHelper {
    
    /**
     *
     * @param c
     * @param t
     * @return
     */
    private[ct] def isSimple (c : blackbox.Context)(t : c.universe.Type) =
        t <:< c.weakTypeOf[AnyVal] || t <:< c.weakTypeOf[String] || t <:< c.weakTypeOf[Number]
    
    /**
     *
     * @param c
     * @param fullTypeName
     * @tparam A
     * @return
     */
    private[ct] def isMacroCallingEnclosingClass[A] (c : blackbox.Context, fullTypeName : String): Boolean =
        try {
            c.reifyEnclosingRuntimeClass.tpe.toString.contains(fullTypeName)
        } catch {
            case _ : NullPointerException => false
        }
    
    /**
     * @param tpe the type for which the type parameters will be extracted
     * @return All TypeParameters as a List corresponding to tpe
     */
    def getTypeParams (c : blackbox.Context)(tpe: c.universe.Type): List[c.universe.Type] = tpe match {
        case c.universe.TypeRef(_, _, args) => args.asInstanceOf[List[c.universe.Type]]
        case _ => List()
    }
    
    
    
}
