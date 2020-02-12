package jstengel.ezxml.extension

import jstengel.ezxml.extension.mapping.FieldMapping
import jstengel.ezxml.extension.rt.RtEncoder.convertToXML
import jstengel.ezxml.extension.rt.RtDecoder
import jstengel.ezxml.extension.mapping.FieldMapping
import jstengel.ezxml.extension.rt.RtDecoder

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
        def obj[T](implicit tt : TypeTag[T], ct: ClassTag[T]): Try[T] = Try(RtDecoder.load[T](elem))
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
