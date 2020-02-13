package jstengel.ezxml.extension.ct

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

/* the field will be derived at runtime and not at compile time.
 * Us this, if the type of a field is uncertain at compile time. */
class RuntimeXML extends StaticAnnotation

// todo maybe inherit function, because there is only one function here
object RuntimeXML {
    
    /**
     * todo description
     * @param c
     * @param annotation
     * @return
     */
    def isRuntimeAnnotation (c: blackbox.Context)(annotation : c.universe.Annotation): Boolean =
        annotation.tree.tpe <:< c.typeOf[RuntimeXML]

}
