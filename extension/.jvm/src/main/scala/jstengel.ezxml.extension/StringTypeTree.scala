package jstengel.ezxml.extension

import StringTypeTree.loadTypeFromClassName

import scala.collection.mutable
import scala.reflect.runtime.universe.{Mirror, Type, TypeRef, internal}
import internal.typeRef


/**
 * This tree corresponds to a type with type parameters.
 * With this class a type can be loaded that is only given as a string
 * @param parentType the ClassName in ClassName[Type1, Type2[Type3] ]
 * @param typeParams everything in the brackets in ClassName[Type1, Type2[Type3] ], but already parsed to span the
 *                   correct tree
 */
private class StringTypeTree (parentType : String, val typeParams : List[StringTypeTree]) {
    /**
     * this function converts the complete tree with it's type parameters to an actual Type
     * @param rm runtime mirror of the current universe. This is given implicitly to reduce boilerplate
     * @return The Type that corresponds to the complete [[StringTypeTree]]
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
     * this apply method creates a StringTypeTree by parsing a className with type parameters
     * @param s the name of the class with tParams in the form of:
     *          ClassName[Type1, Type2[Type3] ]
     * @return a StringTypeTree, with which you can load the type/class associated with the given string
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
     * @param className the name of the class that will be loaded.
     *                  This name is without any brackets or type parameters - just the fullName
     * @param rm the runtime mirror
     * @return the type corresponding to the given class name
     */
    private def loadTypeFromClassName (className : String)(implicit rm : Mirror) : Type =
        try
            rm.staticClass(className).toType
        catch {
            case _ : ScalaReflectionException => rm.staticModule(className).typeSignature
        }
    
}