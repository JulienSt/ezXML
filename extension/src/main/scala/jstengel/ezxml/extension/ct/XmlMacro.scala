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
        import c.universe.{Flag, Modifiers, Quasiquote, TermName, Tree, ValDef}

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

                /* check for companion object */
                val newTail = tail match {
                    case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: x =>
                        
                        val newParents = addParent(c)(parents,
                                                      "XmlObjectTrait",
                                                      tq"jstengel.ezxml.extension.XmlObjectTrait")
                        
                        val newObjBody = body.asInstanceOf[List[Tree]].filterNot{
                             case q"$mods def decode[..$tparams](...$paramss) : $tpt = $expr" =>
                                 val paramList = paramss.asInstanceOf[List[List[ValDef]]]
                                 paramList.length == 1 &&
                                 paramList.head.length == 1 &&
                                 paramList.head.head.tpt.toString().contains("Elem") &&
                                 tpt.toString().contains(tpname.toString())
                             case q"$mods def unapply[..$tparams](...$paramss) : $tpt = $expr" =>
                                 val paramList = paramss.asInstanceOf[List[List[ValDef]]]
                                 paramList.length == 1 &&
                                 paramList.head.length == 1 &&
                                 (paramList.head.head.tpt.toString().contains("Elem") ||
                                  paramList.head.head.tpt.toString().contains("String")) &&
                                 tpt.toString().contains("Option")
                             case _ => false
                        }
                        
                        val existsAnyUnapply = body.asInstanceOf[List[Tree]].exists {
                            case q"$mods def unapply[..$tparams](...$paramss) : $tpt = $expr" =>
                                val paramList = paramss.asInstanceOf[List[List[ValDef]]]
                                paramList.length == 1 &&
                                paramList.head.length == 1 &&
                                paramList.head.head.tpt.toString().contains("Any") &&
                                tpt.toString().contains("Option")
                        }
                        
                        q"""$mods object $tname extends { ..$earlydefns } with ..$newParents { $self =>
                                ${createEncode(c)(tpname, tparams)}
                                ${createXmlUnapply(c)(tpname, paramss)}
                                ${createStringUnapply(c)(paramss)}
                                ${
                                    // an unapply on any is wide spread enough, that we don't want to overwrite it
                                    if (existsAnyUnapply)
                                        q""
                                    else
                                        createAnyUnapply(c)(paramss)
                                }
                                ..$newObjBody
                            }""" :: x
                        
                    case _ =>
                        q"""object ${TermName(tpname.toString)} extends jstengel.ezxml.extension.XmlObjectTrait {
                                ${createEncode(c)(tpname, tparams)}
                                ${createXmlUnapply(c)(tpname, paramss)}
                                ${createStringUnapply(c)(paramss)}
                                ${createAnyUnapply(c)(paramss)}
                            }""" :: Nil
                }
                
                val newParents = addParent(c)(parents,
                                              "XmlClassTrait",
                                              tq"jstengel.ezxml.extension.XmlClassTrait")
                
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
                        ${createExtraction(c)(paramss)}
                        ..$newClassBody
                    }
                    ..$newTail"""
                
            case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a class")
        }
        c.Expr[Any](resultAsTree)
    }
    
    def addParent(c : whitebox.Context)(parents: List[c.Tree], className: String, typeTree: c.Tree): List[c.Tree] = {
        assert(typeTree.toString.contains(className))
        if (parents.exists(_.toString.contains(className)))
            parents
        else
            parents ::: List(typeTree)
    }
    
    def createEncode (c : whitebox.Context)(tpname: c.TypeName, tparams: List[c.universe.TypeDef]): c.Tree = {
        import c.universe.Quasiquote
        q"""override def decode(elem: scala.xml.Elem) : $tpname[..$tparams] =
            jstengel.ezxml.extension.ct.CtDecoder.obj[$tpname[..$tparams]](elem)"""
    }
    
    def generateUnapplyType(c : whitebox.Context)(params: List[c.universe.ValDef]): c.Tree = {
        import c.universe.{AppliedTypeTree, Quasiquote, TypeName}
        val constructorTypes = params.map{
            case p if p.tpt.toString.startsWith("_root_.scala.<repeated>") => p.tpt match {
                case AppliedTypeTree(_, args) => AppliedTypeTree(tq"scala.collection.immutable.Seq", args)
            }
            case p => p.tpt
        }
        val constructorTupleType =
            if (constructorTypes.length > 1){
                tq"scala.${TypeName("Tuple" + constructorTypes.length)}[..$constructorTypes]"
            } else if (constructorTypes.length == 1) {
                tq"${ constructorTypes.head }"
            } else {
                tq"Nothing"
            }
        tq"scala.Option[$constructorTupleType]"
    }
    
    def createExtraction(c : whitebox.Context)(paramss: List[List[c.universe.ValDef]]): c.Tree = {
        import c.universe.{Quasiquote, TermName}
        val constructorParams = paramss.head
        q"""def _extractValues(): ${ generateUnapplyType(c)(constructorParams) } = {${
            val paramCalls = constructorParams.map(v => v.name)
            if (constructorParams.length > 1) {
                q"""scala.Some(scala.${ TermName("Tuple" + paramCalls.length) }.${ TermName("apply") }(..$paramCalls))"""
            } else {
                q"scala.Some(${paramCalls.head})"
            }
        }}"""
    }
    
    def createXmlUnapply(c : whitebox.Context)(tpname: c.TypeName, paramss: List[List[c.universe.ValDef]]): c.Tree = {
        import c.universe.Quasiquote
        q"""
            override def unapply(elem: scala.xml.Elem): ${ generateUnapplyType(c)(paramss.head) } =
                if (elem.label.contains(${tpname.toString}))
                    decode(elem)._extractValues()
                else
                    None
        """
    }
    
    def createStringUnapply(c : whitebox.Context)(paramss: List[List[c.universe.ValDef]]): c.Tree = {
        import c.universe.Quasiquote
        q"""
            override def unapply (stringElem: String) : ${ generateUnapplyType(c)(paramss.head) } =
                jstengel.ezxml.core.ElemParser.parseElem(stringElem).flatMap(unapply)
        """
    }
    
    def createAnyUnapply(c : whitebox.Context)(paramss: List[List[c.universe.ValDef]]): c.Tree = {
        import c.universe.Quasiquote
        q"""
            override def unapply (arg : Any) : ${ generateUnapplyType(c)(paramss.head) } = arg match {
                case elem: scala.xml.Elem => unapply(elem)
                case elem: String => unapply(elem)
                case _ => None
            }
        """
    }
    
}