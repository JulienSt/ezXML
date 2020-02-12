package jstengel.ezxml.extension.mapping

import NotNothingObject.NotNothing

import scala.reflect.runtime.universe.TypeTag


/**
 * This class saved correlations between names and accessors for a defined class.
 * @param fieldSubstitutions this map declares which field will be accessed to save a class parameter for reconstruction
 * @tparam C the type for which this Mapping is defined
 */
case class FieldMapping [C : NotNothing] (fieldSubstitutions : (String, String)*)
                                         (implicit private[mapping] val targetType : TypeTag[C]) {
    
    /**
     * @param fieldName the name of the field, that you want to get substituted
     * @return the field name of the field that should be accessed instead of the given name.
     *         If there is no substitute, the original name is returned
     */
    def getSubstituteName (fieldName : String) : String = fieldSubstitutions.find(_._1 == fieldName)
                                                                            .map(_._2)
                                                                            .getOrElse(fieldName)
    
}

object FieldMapping {
    
    type FieldMappings = Seq[FieldMapping[_]]
    
    implicit class FieldMappingsWrapper(maps : FieldMappings) {
    
        /* ---------- this section ensures, that only one mapping per class exists ---------- */
        private[this] val doubles = maps.filter(m => maps.count(_.targetType == m.targetType) != 1)
        assert(doubles.isEmpty,
               s"Only use one mapping per class, you used: \n ${
                   doubles.map(_.targetType)
                          .mkString(",\n")
               } more than once")
        /* ---------------------------------------------------------------------------------- */
    
        /**
         * searches through all the mappings and finds the correct substitution,
         * if there exists something for the given field
         * @param targetType the type of which the field is part of
         * @param fieldName  the name of the field that will be substituted
         * @return the name of the substitution method/field for the given fieldName inside the target type
         */
        def getSubstituteName[C] (fieldName : String)(implicit targetType : TypeTag[C]) : String =
            maps.find(_.targetType == targetType)
                .map(_.getSubstituteName(fieldName))
                .getOrElse(fieldName)
    }
    
}
