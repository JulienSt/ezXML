package jstengel.ezxml.extension.ct

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.{blackbox, whitebox}


/* mark a field outside the constructor and it is saved, even though it is not part of the init params */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class CacheXML extends StaticAnnotation{
    def macroTransform(annottees: Any*): Any = macro CacheXMLMacro.impl
}

/**
 * this static annotation is used within the decoder and encoder
 * to signify which field is cached outside the constructor
 * @param fieldName the name of the original field, that is stored within the cache
 */
class Cached(fieldName: String) extends StaticAnnotation

object CacheXMLMacro {
    
    /**
     * todo description
     * @param c
     * @param annottees
     * @return
     */
    def impl (c : whitebox.Context)(annottees : c.Expr[Any]*) : c.Expr[Any] = {
        import c.universe.{Flag, Modifiers, Quasiquote, TermName, addFlagOps}
        val resultAsTree = annottees map ( _.tree ) match {
            case q"$mods val $fName: $t = $expr" :: tail =>
                val originalMods = mods.asInstanceOf[Modifiers]
                val newMods      = Modifiers(originalMods.flags | Flag.LAZY,
                                             originalMods.privateWithin,
                                             originalMods.annotations)
                val newTerm      = TermName(s"__${ fName.toString }Cache")
                val fNameString  = fName.toString
                if ( isTypeDefined(t) )
                    List(q"@jstengel.ezxml.extension.ct.Cached($fNameString) private var $newTerm: Option[$t] = None",
                         q"$newMods val $fName: $t = $newTerm.getOrElse($expr)") ::: tail
                else
                    List(q"@jstengel.ezxml.extension.ct.Cached($fNameString) private var $newTerm: Option[Any] = None",
                         q"$newMods val $fName = $newTerm.getOrElse($expr)") ::: tail
            
            case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a value inside the body of a class")
        }
        println(resultAsTree)
        c.Expr[Any](q"..$resultAsTree")
    }
    
    /**
     * todo description
     * @param tree
     * @return
     */
    def isTypeDefined (tree : Trees#Tree) : Boolean = !tree.toString.contains("<type ?>")
    
    /**
     * todo description
     * @param c
     * @param annotation
     * @return
     */
    def isCacheAnnotation (c: blackbox.Context)(annotation : c.universe.Annotation): Boolean =
        annotation.tree.tpe <:< c.typeOf[Cached]
    
    /**
     * todo description
     * @param c
     * @param annotation
     * @return
     */
    def getOriginalField (c : blackbox.Context)(annotation : c.universe.Annotation) : String = {
        import c.universe.Quasiquote
        annotation match {
            case q"new $tpname($param)" => param.toString
            case _                      => ""
        }
    }
    
}
