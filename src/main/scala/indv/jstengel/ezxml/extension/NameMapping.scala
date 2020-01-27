package indv.jstengel.ezxml.extension


import scala.util.{Failure, Try}

/**
 * This class saved correlations between names and accessors for a defined class.
 * @param className the class for which this Mapping is defined
 * @param newName an optional new name for the given class under which xml-elements will be saved
 * @param fieldNameMap this map declares under which names the fields will be saved inside the xml
 * @param fieldAccessMap this map declares which field will be accessed to save a class parameter for reconstruction
 */
case class NameMapping (className      : String,
                        newName        : Option[String] = None,
                        fieldNameMap   : Map[String, String] = Map(),
                        fieldAccessMap : Map[String, String] = Map()) {
    
    /* ---------- this section ensures, that only one mapping per field exists --------- */
    private[this] val keyDoubles = fieldNameMap.keys.filter(k => fieldNameMap.keys.count(_ == k) != 1)
    assert(keyDoubles.isEmpty,
           s"Only use one mapping per field, you used: \n ${keyDoubles.mkString("\n")} more than once")
    /* ----------- this section ensures, that each field has it's own result ----------- */
    private[this] val resDoubles = fieldNameMap.values.filter(v => fieldNameMap.values.count(_== v) != 1)
    assert(resDoubles.isEmpty,
           s"Don't map two fields to the same name, \n ${
               resDoubles.map(v => "the fields: \n\t" +
                                   fieldNameMap.filter(_._2 == v)
                                               .keys
                                               .mkString(", ") +
                                   s" used the same value: $v")
                         .mkString("\n")
           }")
    /* ---------------------------------------------------------------------------------- */
    
    /**
     * @return The new name for the defined class or the original name, if there is no other name defined
     */
    def mapName : String = newName.getOrElse(className)
    
    /**
     * @param field the name of the field that will be saved
     * @return the new name for the given field
     */
    def mapFieldName (field: String) : String = fieldNameMap.getOrElse(field, field)
    
    /**
     * @param mapping the name under which a field was saved
     * @return the original field name for the given name
     */
    def originalFieldName (mapping: String) : Option[String] = fieldNameMap.find(_._2 == mapping).map(_._1)
    
    /**
     * @param accessorName the name of the field, that you might want to get substituted
     * @return the field name of the field that should be accessed instead of the given name.
     *         If there is no substitute, the original name is returned
     */
    def mapAccessName (accessorName: String) : String = fieldAccessMap.getOrElse(accessorName, accessorName)
    
    /**
     * merges two different Name mappings, if both mappings adhere to the same class.
     * This mapping has priority over the second Mapping if there is any kind of duplicate
     * @param other the name mapping with which this name mappings will be merged
     * @return a new NameMapping with all non conflicting rules from the prior NameMappings
     */
    def merge(other: NameMapping): Try[NameMapping] =
        if (className == other.className){
            ???
        } else
            Failure(new Exception)  // todo erstelle "class name not match Exception"
}
