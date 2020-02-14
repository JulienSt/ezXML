package jstengel.ezxml.extension.ct

import scala.annotation.StaticAnnotation

/* the field will be derived at runtime and not at compile time.
 * Us this, if the type of a field is uncertain at compile time. */
class RuntimeXML extends StaticAnnotation
