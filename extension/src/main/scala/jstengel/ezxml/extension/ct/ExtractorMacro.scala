package jstengel.ezxml.extension.ct


import scala.reflect.macros.blackbox


object ExtractorMacro {
    
    
    def extractor[A](a: A): Option[_] = macro extractorImpl[A]

    // TODO need to add warning for outside use
    def extractorImpl[A](c: blackbox.Context)
                        (a: c.Expr[A])
                        (implicit ATag : c.WeakTypeTag[A]) : c.Expr[Option[_]] = {
        import c.universe._
        val aType = ATag.tpe
        val functionCalls =
            aType.decls
                 .collectFirst{ case m : c.universe.MethodSymbol if m.isPrimaryConstructor => m }
                 .get
                 .paramLists
                 .head
                 .map( s => q"$a.${TermName(s.toString)}" )
        c.Expr[Option[_]](q"scala.Some(${"scala.Tuple" + functionCalls.size}(..$functionCalls))")
    }
    
}
