package jstengel.ezxml.extension.ct

import jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import jstengel.ezxml.extension.XmlClassTrait
import jstengel.ezxml.extension.ct.CtDecoder.obj

class BaseClass(val a: String)

case class ChildClass(override val a : String) extends BaseClass(a)

@Xml class CompleteAnnotationExample (val s             : String,
                                      @RuntimeXML val b : BaseClass,
                                      c1                : Int) {

    @CacheXML private[ct] val someHardCalculation: Int = {
        // if you look at the output, you will notice that the intermediary steps are missing
        // and only the result gets loaded
        println("calculated")
        println("calculated2")
        println("calculated3")
        1
    }
    @CacheXML val someHardCalculation2: Int = 2

    @CacheXML @RuntimeXML val baseClass: BaseClass = ChildClass("test")

}

object ExampleTest extends App {

    val original = new CompleteAnnotationExample("testString", ChildClass("ImplementedClass-output"), 1414)
    val x        = original.asInstanceOf[XmlClassTrait].encode
    println(x.toPrettyXMLString)
    val decoded = obj[CompleteAnnotationExample](x)
    println(decoded.someHardCalculation)
    println(decoded.someHardCalculation2)
    println(decoded.baseClass)

}