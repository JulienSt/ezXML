package jstengel.ezxml.extension

/* Since Brackets are not really available for type definition, different chars need to be used.
 * This object is used for easier coherency. */
object XmlBracketDefinition {
    /* defs are for compile time */
    def openBracket  : String = "-_"
    def closeBracket : String = "_-"
    def separateType : String = ".."
    
    /* vals are for runtime */
    val OpeningBracket : String = openBracket
    val ClosingBracket : String = closeBracket
    val TypeSeparator  : String = separateType
}

