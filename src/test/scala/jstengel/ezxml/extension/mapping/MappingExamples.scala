package jstengel.ezxml.extension.mapping

import jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import jstengel.ezxml.extension.ct.CtEncoder
import jstengel.ezxml.extension.rt.{RtDecoder, RtEncoder}

import scala.xml.{Elem, Text, UnprefixedAttribute}

/* with this short example the prints should illustrate how the mapping works step by step */
object MappingExamples extends App {
    
    class A(field: Int) {
        val substitution: Int = field
        override def toString : String = s"A($field)"
        override def equals (obj : Any) : Boolean = obj match {
            case other: A => field == other.substitution
        }
    }
    
    val a = new A(23)
    val map = FieldMapping[A]("field" -> "substitution")
    val xml = RtEncoder.convertToXML(a, Seq(map))
    println(a)
    println(xml)
    println(RtDecoder.load[A](xml))
    
    /* The scala.xml chain is a perfect example as a use case, since we can't change the privacy of fields
       after importing the library, therefor a mapping is needed to accurately save the objects */
    
    case class B (a: Int, b:  String)
    val b = B(123, "test")
    
    val maps = Seq(FieldMapping[scala.xml.Elem]("attributes1" -> "attributes"),
                   FieldMapping[scala.xml.Text]("data" -> "text"),
                   FieldMapping[scala.xml.UnprefixedAttribute]("next1" -> "next"))
    val singleXml : Elem = CtEncoder.xml(b)
    val doubleXML : Elem = CtEncoder.xml(CtEncoder.xml(b), maps)
    
    println(singleXml.toPrettyXMLString)
    println(doubleXML.toPrettyXMLString)
    println(RtDecoder.load[Elem](doubleXML))
    println(RtDecoder.load[B](RtDecoder.load[Elem](doubleXML)))
    
}
