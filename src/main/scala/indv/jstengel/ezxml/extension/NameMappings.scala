package indv.jstengel.ezxml.extension


class NameMappings(val mappings: List[NameMapping]) {
    
    /* ---------- this section ensures, that only one mapping per class exists ---------- */
    private[this] val doubles = mappings.filter(m => mappings.count(_.className == m.className) != 1)
    assert(doubles.isEmpty,
           s"Only use one mapping per class, you used: \n ${doubles.map(_.className)
                                                                   .mkString(",\n")} more than once")
    /* ---------- this section ensures, that each class has it's own new Name ---------- */
    private[this] val resDoubles = mappings.filter(_.newName.forall(n => mappings.count(_.newName.contains(n)) != 1))
    assert(resDoubles.isEmpty,
           s"Don't map two fields to the same name, \n ${
               resDoubles.map(v => "the fields: \n\t" +
                                   mappings.filter(_.newName.contains(v))
                                               .map(_.className)
                                               .mkString(", ") +
                                   s" used the same value: ${v.newName.get}")
                         .mkString("\n")
           }")
    /* ---------------------------------------------------------------------------------- */
    
    /* This function is used to identify the correct class inside all NameMappings based on a given ClassName */
    val isCorrectClass: String => NameMapping => Boolean =
        className => nameMapping =>
            nameMapping.className == className ||
            nameMapping.newName.contains(className)
        
    
    def mapClassName(className: String): String =
        mappings.find(_.className == className)
                .map(_.mapName)
                .getOrElse(className)
    
    /**
     *
     * @param className
     * @param fieldName
     * @return
     */
    def mapFieldName(className: String, fieldName: String): String =
        mappings.find(isCorrectClass(className))
                .map(_.mapFieldName(fieldName))
                .getOrElse(fieldName)
    
    /**
     *
     * @param className
     * @param accessorName
     * @return
     */
    def mapAccessName(className: String, accessorName: String): String =
        mappings.find(isCorrectClass(className))
                .map(_.mapAccessName(accessorName))
                .getOrElse(accessorName)
    
    /**
     *
     * @param mappedName
     * @return
     */
    def originalClassName(mappedName: String): Option[String] =
        mappings.find(_.newName.contains(mappedName))
                .map(_.className)
}
