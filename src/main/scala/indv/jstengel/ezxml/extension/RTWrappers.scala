package indv.jstengel.ezxml.extension


import indv.jstengel.ezxml.extension.mapping.FieldMapping
import indv.jstengel.ezxml.extension.rt.RTConverter.convertToXML
import indv.jstengel.ezxml.extension.rt.RTLoader

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.Try
import scala.xml.Elem

object RTWrappers {
    
    /**
     *
     * @param elem
     */
    implicit class ElemWrapper (elem : Elem) {
        import scala.reflect.runtime.universe.TypeTag
        /**
         * Tries to load the given elem as object of type T
         * @tparam T
         * @return
         */
        def obj[T](implicit tt : TypeTag[T], ct: ClassTag[T]): Try[T] = Try(RTLoader.load[T](elem))
    }
    
    /**
     *
     * @param t
     * @param tt
     * @param ct
     * @tparam T
     */
    implicit class ObjWrapper[T](t: T)(implicit tt : TypeTag[T], ct : ClassTag[T]) {
    
        /**
         *
         * @return
         */
        def xml: Elem = convertToXML(t)
    
        /**
         *
         * @param mappings
         * @return
         */
        def xml(mappings : FieldMapping[_]*): Elem = convertToXML(t, mappings)
        
    }
}
