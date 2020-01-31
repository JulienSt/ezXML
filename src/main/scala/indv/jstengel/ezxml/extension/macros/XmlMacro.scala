package indv.jstengel.ezxml.extension.macros

import indv.jstengel.ezxml.extension.macros.SimpleAnnotations.isValid

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox.Context

// https://github.com/norcane/reminder/blob/master/src/main/scala/com/norcane/reminder.scala
// https://stackoverflow.com/questions/21032869/create-or-extend-a-companion-object-using-a-macro-annotation-on-the-class

@compileTimeOnly("enable macro paradise to expand macro annotations")
class Xml extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro XMLMacro.impl
}

object XMLMacro {
    def impl (c : Context)(annottees : c.Expr[Any]*) : c.Expr[Any] = {
        import c.universe._
        val result = annottees map ( _.tree ) match {
            case all @ q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns }
                     with ..$parents { $self => ..$stats } """ :: tail =>
                
                /* retrieve all the fields outside the constructor with annotations */
                val newStats = stats.asInstanceOf[List[Tree]].map{
                    case stat @ q"$mods val $fieldName: $tpt = $expr" =>
            
                        val originalMods = mods.asInstanceOf[Modifiers]
                        val newMods = Modifiers(originalMods.flags | Flag.LAZY,
                                                originalMods.privateWithin,
                                                originalMods.annotations)
            
                        val annotations = mods
                            .asInstanceOf[Modifiers]
                            .annotations
                            .flatMap{
                                case q"new $annot($param)" if isValid(annot.toString) => Some(annot, Some(param))
                                case q"new $annot" if isValid(annot.toString)         => Some(annot, None)
                                case _                                                => None
                            }
            
                        /* no annotation shall be used more than once */
                        val doubles = annotations.filter(a => annotations.count(_._1.toString == a._1.toString) != 1)
                        assert(doubles.isEmpty,
                               "\nOnly use a Annotation once per field, you used: \n " +
                               s"${ doubles.map(_._1).mkString(",\n") } more than once in class $tpname on $fieldName")
            
                        /* If a type is given, the type is used. when none is given, the runtime-type will be used */
                        if ( annotations.exists(_._1.toString.contains(SimpleAnnotations.cacheAnnot)) ) {
                            Some(fieldName -> annotations)
                            val newTerm      = TermName(s"__${ fieldName.toString }Cache")
                            val xmlExpansion =
                                q"""indv.jstengel.ezxml.extension.macros.CTConverter.xml($fieldName,
                                                                                         ${fieldName.toString})"""
                
                            val newDefinitions =
                                if ( isTypeDefined(tpt) )
                                    List(q"private[$tpname] var $newTerm: Option[$tpt] = None",
                                         q"$newMods val $fieldName: $tpt = $newTerm.getOrElse($expr)")
                                else
                                    List(q"private[$tpname] var $newTerm: Option[Any] = None",
                                         q"$newMods val $fieldName = $newTerm.getOrElse($expr)")
                
                            (newDefinitions, xmlExpansion)
                        } else
                              (List(stat), None)
                    case stat                                         =>
                        (List(stat), None)
                }
                
                /* check for companion object */
//                val newTail = tail
                val newTail = tail match {
                    case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: _ =>
                        q"""$mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
                                def loadFromXML(elem: scala.xml.Elem) : $tpname[..$tparams] =
                                    indv.jstengel.ezxml.extension.macros.CTLoader.obj[$tpname[..$tparams]](elem)
                            ..$body
                            }"""
                    case _ =>
                        q"""object ${TermName(tpname.toString)} {
                                def loadFromXML(elem: scala.xml.Elem) : $tpname[..$tparams] =
                                    indv.jstengel.ezxml.extension.macros.CTLoader.obj[$tpname[..$tparams]](elem)
                            }"""
                }
                println(newTail)
                
                q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents {
                        $self =>
                        def saveAsXml : scala.xml.Elem = indv.jstengel.ezxml.extension.macros.CTConverter.xml(this)
                        ..${newStats.flatMap(_._1)}
                    }
                    ..$newTail"""
                
            case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: _ =>
                q"""$mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
                    ..$body
                }"""
            case _ => c.abort(c.enclosingPosition, "Invalid annotation target: not a class")
        }
//        println(result)
        c.Expr[Any](result)
    }
    
    def isTypeDefined(tree: Trees#Tree): Boolean = !tree.toString.contains("<type ?>")
    
}