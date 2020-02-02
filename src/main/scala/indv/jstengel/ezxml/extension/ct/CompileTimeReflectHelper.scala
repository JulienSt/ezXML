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
    
    
    private[ct] def mapNameAsExpr[A] (c : blackbox.Context)
                                     (mapFieldNames : Option[c.Expr[(String, String) => Option[String]]],
                                  fieldName     : Option[c.Expr[String]],
                                  typeAsExpr    : c.Expr[String]) : c.Expr[String] = { import c.universe._
        (fieldName, mapFieldNames) match {
            case (None, _)                            => c.Expr[String](q"null")
            case (Some(fieldName), None)              => c.Expr[String](q"$fieldName")
            case (Some(fieldName), Some(mappingExpr)) =>
                c.Expr[String](q"""$mappingExpr($typeAsExpr, $fieldName).getOrElse($fieldName)""")
        }
    }
    
}
