package indv.jstengel.ezxml


//import indv.jstengel.ezxml.extension.macros.CTConverter
//import indv.jstengel.ezxml.extension.reflection.RTConverter.convertToXML
//import scala.reflect.runtime.universe.TypeTag
//import scala.reflect.ClassTag
//import scala.xml.Elem

//// todo in extra compile packen
//object AnnotationWrapper { //todo in extra gradle compilation verschieben
//    implicit class ObjWrapper[T](t: T)(implicit tt : TypeTag[T], ct : ClassTag[T]) {
//        def xml: Elem = CTConverter.compileXML(t)
////        def xml: Elem = Converter.convertToXML(t, (_, _) => None)
//        def xml(prefix: String): Elem = CTConverter.compileXML(t, prefix)
////        def xml(prefix: String): Elem = Converter.convertToXML(t, (_, _) => None, prefix)
//        def xmlWithMapping(mapFieldName: (String, String) => Option[String]): Elem =
//            convertToXML(t, mapFieldName)
//    }
//}
