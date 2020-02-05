package indv.jstengel.ezxml.extension


import indv.jstengel.ezxml.extension.mapping.FieldMappings
import indv.jstengel.ezxml.extension.rt.RTConverter.convertToXML
import indv.jstengel.ezxml.extension.rt.RTLoader

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.Try
import scala.xml.Elem

object RTWrappers {
    implicit class ElemWrapper (elem : Elem) {
        import scala.reflect.runtime.universe.TypeTag
        /**
         * Tries to load the given elem as object of type T
         * @tparam T
         * @return
         */
        def obj[T](implicit tt : TypeTag[T], ct: ClassTag[T]): Try[T] = Try(RTLoader.load[T](elem))
    }
    implicit class ObjWrapper[T](t: T)(implicit tt : TypeTag[T], ct : ClassTag[T]) {
        def xml: Elem = convertToXML(t)
        def xml(prefix: String): Elem = convertToXML(t, pre = prefix)
        def xml(mappings : FieldMappings): Elem = convertToXML(t, mappings)
    }
}
