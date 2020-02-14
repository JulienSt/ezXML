package jstengel.ezxml.core

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import SimpleWrapper._

import scala.xml.Elem

//@RunWith(classOf[JUnitRunner])
//class WrapperTests extends FlatSpec {
//    val elem
//}

object WrapperTests extends App {
    val elem: Elem = <A/>
    
    println(elem.setAttribute("test", "1").toPrettyXMLString)
    println(elem.setAttributes(("test1", "1"), ("test2", "2")).toPrettyXMLString)
    println(elem.setAttWithPre("pre1", "test1", "1").toPrettyXMLString)
    println(elem.setAttsWithPre(("pre1", "test1", "1"), ("pre2", "test2", "2")).toPrettyXMLString)
    
}
