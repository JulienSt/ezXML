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
        val aType = ATag.tpe
        val tempName = TermName("__tempName__")
        val functionCalls =
            aType.decls
                 .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                 .get
                 .paramLists
                 .head
                 .map( s => q"$tempName.${s.asInstanceOf[TermSymbol].name}" )
        val t =
            q"""{
               val $tempName = $a
               scala.Some(scala.${TermName("Tuple" + functionCalls.size)}.${TermName("apply")}(..$functionCalls))
               }"""
//        println(t)
        c.Expr[Option[B]](t)
    }
    
}
