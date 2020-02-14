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

//noinspection DuplicatedCode
object CacheXMLMacro {
    
    def impl (c : whitebox.Context)(annottees : c.Expr[Any]*) : c.Expr[Any] = {
        import c.universe.{Flag, Modifiers, Quasiquote, TermName, addFlagOps}
        val resultAsTree = annottees map ( _.tree ) match {
            case q"$mods val $fName: $tpe = $expr" :: tail =>
                val fNameString  = fName.toString
                val newTerm      = TermName(s"__${fNameString}Cache")
                val originalMods = mods.asInstanceOf[Modifiers]
                val newMods      = Modifiers(originalMods.flags | Flag.LAZY,
                                             originalMods.privateWithin,
                                             originalMods.annotations)
                if ( isTypeDefined(tpe) )
                    List(q"""var $newTerm: Option[$tpe] = None""",
                         q"$newMods val $fName: $tpe = $newTerm.getOrElse($expr)") ::: tail
                else
                    List(q"""var $newTerm: Option[Any] = None""",
                         q"$newMods val $fName = $newTerm.getOrElse($expr)") ::: tail
            case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a value inside the body of a class")
        }
        c.Expr[Any](q"..$resultAsTree")
    }
    
    /**
     * @param tree the tree that represents the type of the original field
     * @return true if the type is defined at macro expansion time, false if not
     */
    private def isTypeDefined (tree : Trees#Tree) : Boolean = !tree.toString.contains("<type ?>")
    
    /**
     * Identifies a field of a class as caching field (created by [[CacheXML]]) and associates the correct original
     * field with the cache
     * @param c context, to access types and symbols, during compile time
     * @param symbol the symbol, that is checked, if it is a cache or not
     * @return Some(field-symbol, original field name), if the symbol is of a caching field
     *         None, if the symbol is not of a caching field
     */
    def mapCache(c: blackbox.Context)(symbol: c.Symbol): Option[(c.Symbol, String)] = {
        val symbolName = symbol.name.toString
        if (symbolName.startsWith("__") && symbolName.endsWith("Cache"))
            Some((symbol, symbolName.drop(2).dropRight(5)))
        else
            None
    }
    
}
