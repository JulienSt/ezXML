package indv.jstengel.ezxml.extension.mapping


import indv.jstengel.ezxml.extension.ct.CTConverter
import indv.jstengel.ezxml.extension.rt.{RTConverter, RTLoader}
import indv.jstengel.ezxml.core.SimpleWrapper.NodeWrapper

import scala.xml.{Elem, Text, UnprefixedAttribute}


object MappingTest extends App {

    class A(field: Int) {
        val substitution: Int = field * 2
    
        override def toString : String = s"A($field)"
    }

    val a = new A(23)
    
    val map = FieldMapping[A]("field" -> "substitution")
    
    val xml = RTConverter.convertToXML(a, Seq(map))
    
    println(a)
    println(xml)
    println(RTLoader.load[A](xml))
    
    case class B (a: Int, b:  String)
    
    val b = B(123, "test")
    
//    println(RTLoader.load[B](RTLoader.load[Elem](CTConverter.xml(CTConverter.xml(b), Seq(
//        FieldMapping[Elem]("attributes1" -> "attributes"),
//        FieldMapping[Text]("data" -> "text"),
//        FieldMapping[UnprefixedAttribute]("next1" -> "next")
//    )))))
    
//    println(CTConverter.xml(b).toPrettyXMLString)
//
//    println(CTConverter.xml(CTConverter.xml(b), FieldMappings(
//        FieldMapping[scala.xml.Elem]("attributes1" -> "attributes"),
//        FieldMapping[scala.xml.Text]("data" -> "text"),
//        FieldMapping[scala.xml.UnprefixedAttribute]("next1" -> "next")
//    )).toPrettyXMLString)
//
//    println(RTLoader.load[Elem](CTConverter.xml(CTConverter.xml(b), FieldMappings(
//        FieldMapping[scala.xml.Elem]("attributes1" -> "attributes"),
//        FieldMapping[scala.xml.Text]("data" -> "text"),
//        FieldMapping[scala.xml.UnprefixedAttribute]("next1" -> "next")
//    ))).toPrettyXMLString)
}
