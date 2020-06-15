package jstengel.ezxml.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox


/**
 * this macro prepares an annotated class, so that it automatically has encoding and decoding functions
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class Xml extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro XMLMacro.impl
}

//noinspection DuplicatedCode
object XMLMacro {
    
    def impl (c : whitebox.Context)(annottees : c.Expr[Any]*): c.Expr[Any] = {
        import c.universe.{Quasiquote, TermName, Tree}
        
        val resultAsTree = annottees map ( _.tree ) match {
            case q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents
                     { $self => ..$stats } """ :: tail =>
                
                if (tparams.asInstanceOf[List[Tree]].nonEmpty)
                    c.abort(c.enclosingPosition,
                            """The class you are trying to annotate has type parameters.
                              |Due to compilation order, the necessary type information to create the methods
                              |is not available. Therefore you have to remove the @Xml annotation again
                              |for your program to compile.
                              |If you need to create the function at compile time and cannot use the runtime variants,
                              |you can simply use CtEncoder.xml or CtDecoder.obj""".stripMargin)
                
                val parentList = parents.asInstanceOf[List[Tree]]
                val newParents = if (parentList.exists(_.toString.contains("XmlClassTrait")))
                                     parentList
                                 else
                                     parentList ::: List(tq"jstengel.ezxml.macros.XmlClassTrait")
                
                /* check for companion object */
                val newTail = tail match {
                    case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: _ =>
                        val parentList = parents.asInstanceOf[List[Tree]]
                        val newParents = if (parentList.exists(_.toString.contains("XmlObjectTrait")))
                                             parentList
                                         else
                                             parentList ::: List(tq"jstengel.ezxml.macros.XmlObjectTrait")
                        q"""$mods object $tname extends { ..$earlydefns } with ..$newParents { $self =>
                                override def decode(elem: scala.xml.Elem) : $tpname[..$tparams] =
                                    jstengel.ezxml.macros.ct.CtDecoder.obj[$tpname[..$tparams]](elem)
                            ..$body
                            }"""
                    case _ =>
                        q"""object ${TermName(tpname.toString)} extends jstengel.ezxml.macros.XmlObjectTrait {
                                override def decode(elem: scala.xml.Elem) : $tpname[..$tparams] =
                                    jstengel.ezxml.macros.ct.CtDecoder.obj[$tpname[..$tparams]](elem)
                            }"""
                }
                
                q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with
                    ..$newParents { $self =>
                        override def encode: scala.xml.Elem =
                            jstengel.ezxml.macros.ct.CtEncoder.xmlMacro[$tpname[..$tparams]](this)
                        ..$stats
                    }
                    ..$newTail"""
                
            case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a class")
        }
        c.Expr[Any](resultAsTree)
    }
    
}