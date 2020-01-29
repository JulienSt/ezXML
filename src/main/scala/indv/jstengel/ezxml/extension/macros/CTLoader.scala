package indv.jstengel.ezxml.extension.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.Elem
import indv.jstengel.ezxml.extension.macros.CompileTimeReflectHelper.{isSimple, mapNameAsExpr}


object CTLoader {
    
//    def obj[A]: Elem => A = macro loadObjFromXml[A]
//    def loadObjFromXml[A](c : blackbox.Context)(implicit ATag: c.WeakTypeTag[A]): c.Expr[Elem => A] = {
//
//        import c.universe._
//
//        val aType = ATag.tpe
//        val fullTypeName = aType.typeSymbol.fullName
//        val typeAsExpr = c.Expr[String](q"$fullTypeName")
//
//        if (isSimple(c)(aType))
//            c.Expr[Elem](q"""
//                scala.xml.Elem($mappedName, $typeAsExpr, scala.xml.Null, scala.xml.TopScope, true, Seq(): _*) %
//                    scala.xml.Attribute("value", scala.xml.Text($a.toString()), scala.xml.Null)
//            """)
//
//        // something like new ${name of the class} ( ${fold over params to create code} )
//
//    }
    
}
