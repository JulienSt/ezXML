package indv.jstengel.ezxml.extension

import indv.jstengel.ezxml.extension.AnnotatedExampleClasses.{EmptyIntSet, IntSet, NonEmpty}
import indv.jstengel.ezxml.extension.RTWrappers.ObjWrapper
import indv.jstengel.ezxml.extension.ct.CTLoader.obj


object quicktest extends App {
    
    //    import scala.reflect.{runtime => rt}
    //    import rt.{universe => ru}
    //    import ru._
    //    private implicit val rm: Mirror = rt.currentMirror
    
    
    // todo
    //    println(new IntList(1, 2, 3, 4, 5, 6).xml.toPrettyXMLString)
    //    println(new IntList(1, 2, 3, 4, 5, 6).xml.obj[IntList])
    //    println(new AnnotatedIntList(1, 2, 3, 4, 5, 6).xml.toPrettyXMLString)
    //    println(new AnnotatedIntList(1, 2, 3, 4, 5, 6).xml.obj[AnnotatedIntList])
    //    println(ccIntList(1, 2, 3, 4, 5, 6).xml.toPrettyXMLString)
    //    println(ccIntList(1, 2, 3, 4, 5, 6).xml.obj[ccIntList])
    //    println(new NonEmpty())
    
    //    AnnotatedIntList
    val tree = new NonEmpty(4,
                            new NonEmpty(6,
                                         new NonEmpty(19,
                                                      EmptyIntSet,
                                                      EmptyIntSet),
                                         new NonEmpty(90,
                                                      EmptyIntSet,
                                                      EmptyIntSet)),
                            new NonEmpty(13,
                                         EmptyIntSet,
                                         EmptyIntSet))
    println(tree.xml)
    println(obj[IntSet](tree.xml))
    
}
