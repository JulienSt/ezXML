package jstengel.ezxml.extension.ct

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
        import c.universe.{Quasiquote, TermName, Tree, Modifiers, Flag, ValDef}

        val resultAsTree = annottees map ( _.tree ) match {
            case q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents
                     { $self => ..$classBody } """ :: tail =>

                if (mods.asInstanceOf[Modifiers].hasFlag(Flag.ABSTRACT))
                    c.abort(c.enclosingPosition,
                            """The class you are trying to annotate is abstract.
                              |Abstract classes can not be converted at compile time, because they are not concrete yet.
                              |You can only annotate concrete classes.
                              |Therefore you have to remove the @Xml annotation again
                              |for your program to compile.
                              |If you need to create a conversion at compile time and cannot use the runtime variants,
                              |you can simply use CtEncoder.xml or CtDecoder.obj""".stripMargin)

                if (tparams.asInstanceOf[List[Tree]].nonEmpty)
                    c.abort(c.enclosingPosition,
                            """The class you are trying to annotate has type parameters.
                              |Due to compilation order, the necessary type information to create the needed methods
                              |is not available. Therefore you have to remove the @Xml annotation again
                              |for your program to compile.
                              |If you need to create a conversion at compile time and cannot use the runtime variants,
                              |you can simply use CtEncoder.xml or CtDecoder.obj""".stripMargin)
                
                val parentList = parents.asInstanceOf[List[Tree]]
                val newParents = if (parentList.exists(_.toString.contains("XmlClassTrait")))
                                     parentList
                                 else
                                     parentList ::: List(tq"jstengel.ezxml.extension.XmlClassTrait")

                /* check for companion object */
                val newTail = tail match {
                    case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: x =>
                        val parentList = parents.asInstanceOf[List[Tree]]
                        val newParents = if (parentList.exists(_.toString.contains("XmlObjectTrait")))
                                             parentList
                                         else
                                             parentList ::: List(tq"jstengel.ezxml.extension.XmlObjectTrait")
                        val newObjBody = body.asInstanceOf[List[Tree]].filterNot{
                             case q"$mods def decode[..$tparams](...$paramss) : $tpt = $expr" =>
                                 val paramList = paramss.asInstanceOf[List[List[ValDef]]]
                                 paramList.length == 1 &&
                                 paramList.head.length == 1 &&
                                 paramList.head.head.tpt.toString().contains("Elem") &&
                                 tpt.toString().contains(tpname.toString())
                             case _ => false
                        }
                        q"""$mods object $tname extends { ..$earlydefns } with ..$newParents { $self =>
                                override def decode(elem: scala.xml.Elem) : $tpname[..$tparams] =
                                    jstengel.ezxml.extension.ct.CtDecoder.obj[$tpname[..$tparams]](elem)
                                ..$newObjBody
                            }""" :: x
                    case _ =>
                        q"""object ${TermName(tpname.toString)} extends jstengel.ezxml.extension.XmlObjectTrait {
                                override def decode(elem: scala.xml.Elem) : $tpname[..$tparams] =
                                    jstengel.ezxml.extension.ct.CtDecoder.obj[$tpname[..$tparams]](elem)
                            }""" :: Nil
                }

                // filter out duplicate encoding functions to make intellij happy
                val newClassBody = classBody.asInstanceOf[List[Tree]].filterNot{
                    case q"$mods def encode[..$tparams](...$paramss): $tpt = $expr" =>
                        paramss.head.isEmpty && tpt.toString().contains("Elem")
                    case _ => false
                }

                q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with
                    ..$newParents { $self =>
                        override def encode(): scala.xml.Elem =
                            jstengel.ezxml.extension.ct.CtEncoder.xmlMacro[$tpname[..$tparams]](this)
                        ..$newClassBody
                    }
                    ..$newTail"""
                
            case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a class")
        }
        c.Expr[Any](resultAsTree)
    }
    
}