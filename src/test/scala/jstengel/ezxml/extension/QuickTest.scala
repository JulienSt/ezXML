package jstengel.ezxml.extension

import RTWrappers.{ElemWrapper, ObjWrapper}
import jstengel.ezxml.extension.ct.CtDecoder.obj
import jstengel.ezxml.extension.ct.CtEncoder.{xml, xmlMacro}
import jstengel.ezxml.core.SimpleWrapper.NodeWrapper
import AnnotatedExampleClasses._
import ExampleClasses._

/**
 * this object is used for quick & dirty tests during development
 */
object QuickTest extends App {
    
    //    import scala.reflect.{runtime => rt}
    //    import rt.{universe => ru}
    //    import ru._
    //    private implicit val rm: Mirror = rt.currentMirror
    
    // todo lambdas? but not before the first release
    //    class SpecialTypeParameterTestClass[T1, T2](a: T2, b: T1, c: T1 => T2, d: T2 => T1)
    
}
