package jstengel.ezxml.extension.mapping

import jstengel.ezxml.extension.ct.CtEncoder
import jstengel.ezxml.extension.mapping.MappingExamples.{A, B}
import jstengel.ezxml.extension.rt.{RtDecoder, RtEncoder}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner

import scala.xml.Elem

@RunWith(classOf[JUnitRunner])
class MappingTest extends FlatSpec {

    val a                     = new A(23)
    val map : FieldMapping[A] = FieldMapping[A]("field" -> "substitution")
    s"\n$a" should s" load with $map" in {
        val xml = RtEncoder.convertToXML(a, Seq(map))
        assert(a == RtDecoder.load[A](xml))
    }

    val b         : B    = B(123, "test")
    val maps = Seq(
        FieldMapping[scala.xml.Elem]("attributes1" -> "attributes"),
        FieldMapping[scala.xml.Text]("data" -> "text"),
        FieldMapping[scala.xml.UnprefixedAttribute]("next1" -> "next")
        )
    s"\n$b" should s" load with $maps" in {
        val doubleXML : Elem = CtEncoder.xml(CtEncoder.xml(b), maps)
        assert(b == RtDecoder.load[B](RtDecoder.load[Elem](doubleXML)) )
    }

}