package jstengel.ezxml.extension.ct


import scala.reflect.macros.blackbox
import scala.language.experimental.macros


object ExtractorMacro {
    
    
    def extractor[A, B](a: A): Option[B] = macro extractorImpl[A, B]

    // TODO need to add warning for outside use
    def extractorImpl[A, B](c: blackbox.Context)
                        (a: c.Expr[A])
                        (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Option[B]] = {
        import c.universe._
//        zip type B with the params to find out which param is private and then remove everything, that does not pair up
        val constructorParams =
            ATag.tpe
                .decls
                .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                .get
                .paramLists
                .head
//                .filterNot(_.asInstanceOf[ValDef].mods.hasFlag(Flag.PRIVATE)) does not work with valdef, needs to be TermSymbol
        val t =
            if (constructorParams.length > 1) {
                val tempName = TermName("__tempName__")
                val paramCalls = constructorParams.map( s => q"$tempName.${s.asInstanceOf[TermSymbol].name}" )
                q"""{
                    val $tempName = $a
                    scala.Some(scala.${TermName("Tuple" + paramCalls.length)}.${TermName("apply")}(..$paramCalls))
                }"""
            } else {
                val paramCall = constructorParams.map( s => q"$a.${s.asInstanceOf[TermSymbol].name}" ).head
                q"scala.Some($paramCall)"
            }
//        println(t)
        c.Expr[Option[B]](t)
    }
    
}
