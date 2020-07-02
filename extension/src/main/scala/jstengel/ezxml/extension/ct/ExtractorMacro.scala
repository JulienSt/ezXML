package jstengel.ezxml.extension.ct


import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.{blackbox, whitebox}
import scala.language.experimental.macros

/**
 * this macro prepares an annotated class, so that it automatically has an extractor function
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class Extract extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro ExtractorMacro.impl
}

object ExtractorMacro {
    
    def impl (c : whitebox.Context)(annottees : c.Expr[Any]*): c.Expr[Any] = {
//        import c.universe.{Quasiquote, Tree, TermName, Modifiers, Flag, ValDef, TypeName, TermSymbol}
        import c.universe._
        
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
    
                if (!parents.asInstanceOf[List[Tree]].exists(_.toString.contains("XmlClassTrait")))
                    c.abort(c.enclosingPosition,
                            """The class you are trying to annotate with Extractor has to also be annotated with Xml.
                              |""".stripMargin)
    
                val constructorParams =
                    paramss.asInstanceOf[List[List[Tree]]]
                           .head
                           .flatMap(t => {
                               val valDef = t.asInstanceOf[ValDef]
                               if (valDef.mods.hasFlag(Flag.PRIVATE))
                                   None
                               else
                                   Some(valDef)
                           })
                
                val constructorTypes = constructorParams.map{
                    case p if p.tpt.toString.startsWith("_root_.scala.<repeated>") => p.tpt match {
                        case AppliedTypeTree(_, args) => AppliedTypeTree(tq"scala.collection.immutable.Seq", args)
                    }
                    case p => p.tpt
                }
                
                val constructorTupleType =
                    if (constructorTypes.length > 1){
                        tq"scala.${TypeName("Tuple" + constructorTypes.length)}[..$constructorTypes]"
                    } else {
                        tq"${ constructorTypes.head }"
                    }
                val unapplyReturnTypeTree = tq"scala.Option[$constructorTupleType]"
                
                val tempName = TermName("__tempName__")
                
                /* change companion object */
                val newTail = tail match {
                    case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: x =>
                        val parentList = parents.asInstanceOf[List[Tree]]
                        val newParents = if (parentList.exists(_.toString.contains("XmlExtractorTrait")))
                                             parentList
                                         else
                                             parentList ::: List(tq"jstengel.ezxml.extension.XmlExtractorTrait")
                        val newObjBody = body.asInstanceOf[List[Tree]].filterNot{
                            case q"$mods def unapply[..$tparams](...$paramss) : $tpt = $expr" =>
                                val paramList = paramss.asInstanceOf[List[List[ValDef]]]
                                paramList.length == 1 &&
                                paramList.head.length == 1 &&
                                paramList.head.head.tpt.toString().contains("Elem") &&
                                tpt.toString().contains("Option")
                            case _ => false
                        }
                        q"""$mods object $tname extends { ..$earlydefns } with ..$newParents { $self =>
                                ..$newObjBody
                                def unapply(elem: scala.xml.Elem): $unapplyReturnTypeTree = {
                                    if (elem.label.contains(${tpname.toString()})) {
                                        val $tempName = decode(elem)
                                        ${
                                            if (constructorParams.length > 1) {
                                                val paramCalls = constructorParams.map(v => q"$tempName.${ v.name }")
                                                q"""scala.Some(scala.${ TermName("Tuple" + paramCalls.length) }.${ TermName("apply") }(..$paramCalls))"""
                                            } else {
                                                val paramCall = constructorParams.map( v => q"$tempName.${v.name}" ).head
                                                q"scala.Some($paramCall)"
                                            }
                                        }
                                    } else
                                        None
                                }
                            }""" :: x
                    case _ =>
                        c.abort(c.enclosingPosition, """----ERROR----""".stripMargin)
                }
                
                q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with
                    ..$parents { $self => ..$classBody }
                    ..$newTail"""
            
            case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a class")
        }
        println(resultAsTree)
        c.Expr[Any](resultAsTree)
    }
    
}
