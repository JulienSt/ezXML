package indv.jstengel.ezxml.extension

import indv.jstengel.ezxml.extension.StringTypeTree.loadTypeFromClassName

import scala.collection.mutable
import scala.reflect.runtime.universe.{Mirror, Type, TypeRef, internal}
import internal.typeRef

private[ezxml] class StringTypeTree (parentType : String, val typeParams : List[StringTypeTree]) {
    def toType (implicit rm : Mirror) : Type =
        if ( typeParams.isEmpty )
            loadTypeFromClassName(parentType)
        else
            loadTypeFromClassName(parentType) match {
                case TypeRef(t, s, _) => typeRef(t, s, typeParams.map(_.toType))
            }
}

private[ezxml] object StringTypeTree {
    
    def apply (s : String) : StringTypeTree = {
        if ( s.isEmpty ) throw new Exception
        val (rootType, xmlTypeParamsAsString) = s.span(_ != '[')
        val buf                               = mutable.Buffer[String]()
        val stack                             = mutable.Stack[Char]()
        var bracketCount                      = 0
        xmlTypeParamsAsString.drop(1).dropRight(1).foreach{
            case ' '                      =>
            case c @ '['                  => bracketCount += 1
                                             stack.push(c)
            case c @ ']'                  => bracketCount -= 1
                                             stack.push(c)
            case ',' if bracketCount == 0 => buf.append(stack.popAll().mkString)
            case c                        => stack.push(c)
        }
        /* mkString on Seq() produces "", this would be pushed to the buf and therefore wouldn't be empty anymore
         * therefore the stack is checked beforehand, since popAll produces an empty sequence */
        if ( stack.nonEmpty )
            buf.append(stack.popAll().mkString)
        new StringTypeTree(rootType, if ( buf.nonEmpty ) buf.map(apply).toList else List())
    }
    
    def typeFromString (s : String)(implicit rm : Mirror) : Type = StringTypeTree(s).toType
    
    /* without type params in name */
    private def loadTypeFromClassName (className : String)(implicit rm : Mirror) : Type =
        try
            rm.staticClass(className).toType
        catch {
            case _ : ScalaReflectionException => rm.staticModule(className).typeSignature
        }
    
}