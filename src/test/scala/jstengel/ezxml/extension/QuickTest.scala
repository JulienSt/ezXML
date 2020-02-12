package jstengel.ezxml.extension

import RTWrappers.{ElemWrapper, ObjWrapper}
import jstengel.ezxml.extension.ct.CtDecoder.obj
import jstengel.ezxml.extension.ct.CtEncoder.{xml, xmlMacro}
import jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import AnnotatedExampleClasses._
import ExampleClasses._

import scala.collection.mutable

object QuickTest extends App {
    
    //    import scala.reflect.{runtime => rt}
    //    import rt.{universe => ru}
    //    import ru._
    //    private implicit val rm: Mirror = rt.currentMirror
    
    // todo lambdas? but not before the first release
    //    class SpecialTypeParameterTestClass[T1, T2](a: T2, b: T1, c: T1 => T2, d: T2 => T1)
    
//    val a: AnnotatedTypeParameterTestClass1[Int, String, Double] = new AnnotatedTypeParameterTestClass1(1, "test", 2, 4.56)
//    println(ct.CtEncoder.xml(a).toPrettyXMLString)
//    println(a.xml.toPrettyXMLString)
//                                                                    .obj[])

//    jstengel.ezxml.extension.rt.RuntimeReflectHelper.getType(a)(
//    jstengel.ezxml.extension.rt.RuntimeReflectHelper
//        .tagOf(scala.reflect.runtime.universe.typeOf[Any])(scala.reflect.runtime.currentMirror),
//    scala.reflect.ClassTag(a.getClass), scala.reflect.runtime.currentMirror)
//    new CTSpecialTypeParameterTestClass2("test", 1, 2, 3, 4, 5, 6, 7, 8).xml.toPrettyXMLString
    
    val bufferTest = mutable.Buffer(2, 3, 4)
    println(bufferTest)
    val xml = xmlMacro[mutable.Buffer[Int]](bufferTest)
    println(xml.toPrettyXMLString)
//    println(bufferTest == obj[mutable.Buffer[Int]](xml))
    println(bufferTest == obj[mutable.ArrayBuffer[Int]](xml))
    
    val bufferTest2 = mutable.ArrayBuffer(CC1(1, "test1"), CC1(2, "test2"), CC1(3, "test3"))
    val xml2 = xmlMacro[mutable.ArrayBuffer[CC1]](bufferTest2)
    println(xml2.toPrettyXMLString)
    println(obj[mutable.ArrayBuffer[CC1]](xml2))
    
}
