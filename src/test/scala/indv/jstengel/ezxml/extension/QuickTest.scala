package indv.jstengel.ezxml.extension

import indv.jstengel.ezxml.extension.AnnotatedExampleClasses.{AnnotatedIntList, AnnotatedStrangeIterator, CTSpecialTypeParameterTestClass2}
import indv.jstengel.ezxml.extension.RTWrappers.{ElemWrapper, ObjWrapper}
import indv.jstengel.ezxml.extension.ct.CTLoader.obj
import indv.jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import indv.jstengel.ezxml.extension.ExampleClasses.StrangeIterator

object QuickTest extends App {
    
    //    import scala.reflect.{runtime => rt}
    //    import rt.{universe => ru}
    //    import ru._
    //    private implicit val rm: Mirror = rt.currentMirror
    
    // todo lambdas? but not before the first release
    //    class SpecialTypeParameterTestClass[T1, T2](a: T2, b: T1, c: T1 => T2, d: T2 => T1)
    
    new CTSpecialTypeParameterTestClass2("test", 1, 2, 3, 4, 5, 6, 7, 8).xml.toPrettyXMLString
    
}
