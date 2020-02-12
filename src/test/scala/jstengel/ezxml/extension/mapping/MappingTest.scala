package jstengel.ezxml.extension.mapping

import jstengel.ezxml.extension.rt.{RtDecoder, RtEncoder}
import jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import jstengel.ezxml.extension.ct.CtEncoder
import jstengel.ezxml.extension.rt.{RtDecoder, RtEncoder}

import scala.xml.{Elem, Text, UnprefixedAttribute}

// todo create a unit test for this
object MappingTest extends App {

    class A(field: Int) {
        val substitution: Int = field * 2
    
        override def toString : String = s"A($field)"
    }

    val a = new A(23)
    
    val map = FieldMapping[A]("field" -> "substitution")
    
    val xml = RtEncoder.convertToXML(a, Seq(map))
    
    println(a)
    println(xml)
    println(RtDecoder.load[A](xml))
    
    case class B (a: Int, b:  String)
    
    val b = B(123, "test")
    
//    println(RtDecoder.load[B](RtDecoder.load[Elem](CtEncoder.xml(CtEncoder.xml(b), Seq(
//        FieldMapping[Elem]("attributes1" -> "attributes"),
//        FieldMapping[Text]("data" -> "text"),
//        FieldMapping[UnprefixedAttribute]("next1" -> "next")
//    )))))
    
//    println(CtEncoder.xml(b).toPrettyXMLString)
//
//    println(CtEncoder.xml(CtEncoder.xml(b), FieldMappings(
//        FieldMapping[scala.xml.Elem]("attributes1" -> "attributes"),
//        FieldMapping[scala.xml.Text]("data" -> "text"),
//        FieldMapping[scala.xml.UnprefixedAttribute]("next1" -> "next")
//    )).toPrettyXMLString)
//
//    println(RtDecoder.load[Elem](CtEncoder.xml(CtEncoder.xml(b), FieldMappings(
//        FieldMapping[scala.xml.Elem]("attributes1" -> "attributes"),
//        FieldMapping[scala.xml.Text]("data" -> "text"),
//        FieldMapping[scala.xml.UnprefixedAttribute]("next1" -> "next")
//    ))).toPrettyXMLString)
}
