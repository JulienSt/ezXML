package indv.jstengel.ezxml.extension

import indv.jstengel.ezxml.extension.StringTypeTree.loadTypeFromClassName

import scala.collection.mutable
import scala.reflect.runtime.universe.{Mirror, Type, TypeRef, internal}
import internal.typeRef


/**
 *
 * @param parentType
 * @param typeParams
 */
private class StringTypeTree (parentType : String, val typeParams : List[StringTypeTree]) {
    /**
     *
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @return
     */
    def toType (implicit rm : Mirror) : Type =
        if ( typeParams.isEmpty )
            loadTypeFromClassName(parentType)
        else
            loadTypeFromClassName(parentType) match {
                case TypeRef(t, s, _) => typeRef(t, s, typeParams.map(_.toType))
            }
}

private[extension] object StringTypeTree {
    
    /**
     *
     * @param s
     * @return
     */
    private def apply (s : String) : StringTypeTree = {
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
    
    /**
     * Reflects a type based upon a given string. This String has to include all necessary information about that type,
     * including the complete path and all type parameters.
     * The output of RuntimeReflectHelper.createStringRepresentation is most likely the input for this function
     * @param s the string representation for a type
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @return The correct Type, with all type params based upon the input string
     */
    def typeFromString (s : String)(implicit rm : Mirror) : Type = StringTypeTree(s).toType
    
    /**
     * loads a class statically without type params
     * @param className
     * @param rm
     * @return
     */
    private def loadTypeFromClassName (className : String)(implicit rm : Mirror) : Type =
        try
            rm.staticClass(className).toType
        catch {
            case _ : ScalaReflectionException => rm.staticModule(className).typeSignature
        }
    
}