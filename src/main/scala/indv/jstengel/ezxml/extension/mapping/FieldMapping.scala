package indv.jstengel.ezxml.extension.mapping

import indv.jstengel.ezxml.extension.NotNothingObject.NotNothing

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
