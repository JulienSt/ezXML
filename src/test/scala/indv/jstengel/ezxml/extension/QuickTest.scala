package indv.jstengel.ezxml.extension


import indv.jstengel.ezxml.extension.RTWrappers.{ElemWrapper, ObjWrapper}
import indv.jstengel.ezxml.extension.ct.CTLoader.obj
import indv.jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import indv.jstengel.ezxml.extension.AnnotatedExampleClasses.AnnotatedTypeParameterTestClass1
import indv.jstengel.ezxml.extension.ExampleClasses.StrangeIterator

object QuickTest extends App {
    
    //    import scala.reflect.{runtime => rt}
    //    import rt.{universe => ru}
    //    import ru._
    //    private implicit val rm: Mirror = rt.currentMirror
    
    // todo lambdas? but not before the first release
    //    class SpecialTypeParameterTestClass[T1, T2](a: T2, b: T1, c: T1 => T2, d: T2 => T1)
    
    val a: AnnotatedTypeParameterTestClass1[Int, String, Double] = new AnnotatedTypeParameterTestClass1(1, "test", 2, 4.56)
    println(ct.CTConverter.xml(a).toPrettyXMLString)
//    println(a.xml.toPrettyXMLString)
//                                                                    .obj[])

//    indv.jstengel.ezxml.extension.rt.RuntimeReflectHelper.getType(a)(
//    indv.jstengel.ezxml.extension.rt.RuntimeReflectHelper
//        .tagOf(scala.reflect.runtime.universe.typeOf[Any])(scala.reflect.runtime.currentMirror),
//    scala.reflect.ClassTag(a.getClass), scala.reflect.runtime.currentMirror)
//    new CTSpecialTypeParameterTestClass2("test", 1, 2, 3, 4, 5, 6, 7, 8).xml.toPrettyXMLString
    
    
    
}
