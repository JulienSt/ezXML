package indv.jstengel.ezxml.extension

import indv.jstengel.ezxml.extension.AnnotatedExampleClasses.AnnotatedStrangeIterator
import indv.jstengel.ezxml.extension.RTWrappers.{ElemWrapper, ObjWrapper}
import indv.jstengel.ezxml.extension.ct.CTLoader.obj
import indv.jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import indv.jstengel.ezxml.extension.AnnotatedExampleClasses.AnnotatedIntList
import indv.jstengel.ezxml.extension.ExampleClasses.StrangeIterator

object QuickTest extends App {
    
    //    import scala.reflect.{runtime => rt}
    //    import rt.{universe => ru}
    //    import ru._
    //    private implicit val rm: Mirror = rt.currentMirror
    
    println(new AnnotatedStrangeIterator("testID", List((1, 2), (3, 4), (5, 6))).xml.toPrettyXMLString)
    println(new AnnotatedStrangeIterator("testID", List((1, 2), (3, 4), (5, 6))).xml.obj[AnnotatedStrangeIterator])
    
    println(new StrangeIterator("testID", List((1, 2), (3, 4), (5, 6))).xml.toPrettyXMLString)
    println(new StrangeIterator("testID", List((1, 2), (3, 4), (5, 6))).xml.obj[StrangeIterator])
    
}
