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
     * this class lets the user decode an [[Elem]] into an object, by providing the .obj[T]-method to every
     * [[Elem]], once DecodingWrapper is imported into scope
     * @param elem the [[Elem]] that will be decoded
     */
    implicit class DecodingWrapper (elem : Elem) {
        /**
         * Tries to load the given elem as object of type T
         * @tparam T the type that will be loaded from elem
         * @return a runtime object of type [[T]]
         */
        def obj[T](implicit tt : TypeTag[T], ct: ClassTag[T]): Try[T] = Try(RtDecoder.load[T](elem))
    }
    
    /**
     * This wrapper is for easy encoding of objects.
     * It provides the simple function .xml that can be called on every object, once EncodingWrapper is imported
     * into scope.
     * @param t the object that will be encoded
     * @param tt the TypeTag to T (this will be inserted by the compiler)
     * @param ct the ClassTag to T (this will be inserted by the compiler)
     * @tparam T The type of object that will be encoded. This is a generic instead of [[Any]], to capture the needed
     *           TypeTag and ClassTag
     */
    implicit class EncodingWrapper[T] (t : T)(implicit tt : TypeTag[T], ct : ClassTag[T]) {
       
        /**
         * Encodes t in an ELem
         * @return a xml representation of t
         */
        def xml: Elem = convertToXML(t)
    
        /**
         * Encodes t in an ELem, where fields will be mapped to other fields
         * @param mappings maps between a fields and the intended accessors
         * @return a xml representation of t
         */
        def xml(mappings : FieldMapping[_]*): Elem = convertToXML(t, mappings)
    
        /**
         * Encodes t in an ELem with the given prefix, where fields will be mapped to other fields
         * @param prefix the prefix for the root elem
         * @param mappings maps between a fields and the intended accessors
         * @return a xml representation of t
         */
        def xml(prefix: String, mappings : FieldMapping[_]*): Elem = convertToXML(t, mappings, prefix)
        
    }
}
