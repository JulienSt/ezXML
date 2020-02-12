package jstengel.ezxml.extension

import jstengel.ezxml.extension.mapping.FieldMapping
import jstengel.ezxml.extension.rt.RtDecoder
import jstengel.ezxml.extension.rt.RtEncoder.convertToXML

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.Try
import scala.xml.Elem


/**
 * the following implicit classes allow a natural way to use the runtime encoding and decoding of arbitrary objects
 */
object RTWrappers {
    
    /**
     * this class lets the user decode an [[Elem]] into an object
     * @param elem the [[Elem]] that will be decoded
     */
    implicit class ElemWrapper (elem : Elem) {
        /**
         * Tries to load the given elem as object of type T
         * @tparam T the type that will be loaded from elem
         * @return a runtime object of type [[T]]
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
         * @param mappings maps between a fields and the intended accessors
         * @return
         */
        def xml(mappings : FieldMapping[_]*): Elem = convertToXML(t, mappings)
    
        /**
         *
         * @param prefix the prefix for the root elem
         * @param mappings
         * @return
         */
        def xml(prefix: String, mappings : FieldMapping[_]*): Elem = convertToXML(t, mappings, prefix)
        
    }
}
