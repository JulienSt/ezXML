package jstengel.ezxml.extension.ct

import jstengel.ezxml.extension.XmlClassTrait


abstract class BaseClass {
    def a: String
}

case class ChildClass(a: String) extends BaseClass

@Xml class CompleteAnnotationExample (val s             : String,
                                      @RuntimeXML val b : BaseClass,
                                      c1                : Int) {

    val c: Int = c1*2

    @CacheXML private val someHardCalculation = {
        println("calculated")
        5
    }

    /*
    private[FinalExample] var _someHardCalculationCache: Option[T] = None
    lazy val someHardCalculation = _someHardCalculationCache.getOrElse( das was hier vorher stand )
     */

}

object CompleteAnnotationExample {
    def get(fe: CompleteAnnotationExample) = fe.__someHardCalculationCache
}


// todo also test:
//  app.xml.SubstituteXML
//  xml.SubstituteXML
//  SubstituteXML


// todo test that annotations can be used more than once

object ExampleTest extends App {

    val fe = new CompleteAnnotationExample("miep", ChildClass("ImplementedClass-output"), 1414)
    println(fe.asInstanceOf[XmlClassTrait].encode)

    println("''''''''''")
    println(CompleteAnnotationExample.get(fe))
    println("''''''''''")

}