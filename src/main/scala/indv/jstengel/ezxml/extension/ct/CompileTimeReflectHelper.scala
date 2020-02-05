package indv.jstengel.ezxml.extension.ct


import scala.reflect.macros.blackbox


object CompileTimeReflectHelper {
    
    private[ct] def isSimple (c : blackbox.Context)(t : c.universe.Type) =
        t <:< c.weakTypeOf[AnyVal] || t <:< c.weakTypeOf[String] || t <:< c.weakTypeOf[Number]
    
    
    private[ct] def isMacroCallingEnclosingClass[A] (c : blackbox.Context, fullTypeName : String): Boolean =
        try {
            c.reifyEnclosingRuntimeClass.tpe.toString.contains(fullTypeName)
        } catch {
            case _ : Throwable => false
        } // todo tighter case
    
}
