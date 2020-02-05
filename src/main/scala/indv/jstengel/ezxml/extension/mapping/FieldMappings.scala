package indv.jstengel.ezxml.extension.mapping


import scala.reflect.runtime.universe.TypeTag


case class FieldMappings (maps : FieldMapping[_]*) {
    
    /* ---------- this section ensures, that only one mapping per class exists ---------- */
    private[this] val doubles = maps.filter(m => maps.count(_.targetType == m.targetType) != 1)
    assert(doubles.isEmpty,
           s"Only use one mapping per class, you used: \n ${doubles.map(_.targetType)
                                                                   .mkString(",\n")} more than once")
    /* ---------------------------------------------------------------------------------- */
    
    /**
     * searches through all the mappings and finds the correct substitution,
     * if there exists something for the given field
     * @param targetType the type of which the field is part of
     * @param fieldName the name of the field that will be substituted
     * @return the name of the substitution method/field for the given fieldName inside the target type
     */
    def getSubstituteName[C](fieldName: String)(implicit targetType : TypeTag[C]): String =
        maps.find(_.targetType == targetType)
            .map(_.getSubstituteName(fieldName))
            .getOrElse(fieldName)
    
}
