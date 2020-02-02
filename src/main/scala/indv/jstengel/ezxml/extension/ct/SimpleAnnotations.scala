package indv.jstengel.ezxml.extension.ct

import scala.annotation.StaticAnnotation

class RuntimeXML extends StaticAnnotation // TODO is derived at runtime (convert-function)

class CacheXML extends StaticAnnotation // TODO is saved, even though it is not part of the init params

class AsTextXML extends StaticAnnotation // TODO is saved as <String> insert Text here because it is too long </String>

/**
 * Only use this on a parameter inside a constructor, otherwise this annotation does nothing.
 * Also be aware, that you don't have to use this, if @xml is already applied to the same class,
 * since the macro of @Xml constructs a method that can access the value of the annotated parameter.
 * Therefor this annotation is only useful,
 * if a calculated/runtime value should be saved instead of the originally given value.
 * @param field the field that should be accessed/ saved instead of the annotated parameter
 */
class SubstituteFieldXML (field : String) extends StaticAnnotation // TODO integrate into the compile process

class SubstituteNameXML (name : String) extends StaticAnnotation

object SimpleAnnotations {
    
    /* Names of the classes are acquired this way, to ease refactoring and minimize string-literal usage */
    def subFieldAnnot : String       = classOf[SubstituteFieldXML].getSimpleName
    def subNameAnnot  : String       = classOf[SubstituteNameXML].getSimpleName
    def asTextAnnot   : String       = classOf[AsTextXML].getSimpleName
    def rtAnnot       : String       = classOf[RuntimeXML].getSimpleName
    def cacheAnnot    : String       = classOf[CacheXML].getSimpleName
    def validAnnots   : List[String] = List(subFieldAnnot, subNameAnnot, asTextAnnot, rtAnnot, cacheAnnot)
    
    def isValid (className : String) : Boolean = validAnnots.exists(validAnnot => className.contains(validAnnot))
    
}
