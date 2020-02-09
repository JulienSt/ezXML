package indv.jstengel.ezxml.extension.ct

import CTLoader.obj
import CTConverter.xml
import indv.jstengel.ezxml.extension.ExampleClasses.EmptyCaseClass
import indv.jstengel.ezxml.extension.RTWrappers.{ElemWrapper, ObjWrapper}

object CTLoadingTest extends App {

//    val i = xml(2)
//    println(i)
//    println(obj[Int](i))
//
//    val a: Any = 23
//    val a2 = xml(a)
//    println(a2)
//    println(obj[Int](a2))
    
//    case class Test (a : Int)
//    case class Test2 (s : String)
//    case class Test3 (t : Test)
//    val t = Test(2)
//    val t2 = Test2("testInput")
//    val t3 = Test3(Test(23))
//    val x = xml(t)
//    val x2 = xml(t2)
//    val x3 = xml(t3)
//    println(x)
//    println(x2)
//    println(x3)
//    println(obj[Test](x))
//    println(obj[Test2](x2))
//    println(obj[Test3](x3))
    
//    (elem: scala.xml.Elem) => List(elem.child.map{ case elem : scala.xml.Elem => indv.jstengel.ezxml.extension.ct.CTLoader.obj[]}:_*)
//
//    val i2: java.lang.Integer = 90
//    println(i2 == obj[java.lang.Integer](i2.xml))
//    println(i2 == obj[java.lang.Integer](xml(i2)))
//
//    val s: java.lang.String = "TestInput"
//    println(s == obj[java.lang.String](s.xml))
//    println(s == obj[java.lang.String](xml(s)))
//
//    println(EmptyCaseClass() == obj[EmptyCaseClass](xml(EmptyCaseClass())))
    
    // todo listen, Arrays, Maps and the other stuff that is included in CTConverter
    
    val l = List(1, 2, 3, 4, 5)
    println(obj[List[Int]](l.xml))
    println(obj[List[Int]](xml(l)))

    import scala.runtime.ScalaRunTime.stringOf
    val ar = Array(1, 2, 3, 4, 5)
    println(stringOf(obj[Array[Int]](l.xml)))
    println(stringOf(obj[Array[Int]](xml(l))))
    
    
//    (elem: scala.xml.Elem) => new indv.jstengel.ezxml.extension.AnnotatedExampleClasses.StrangeIterator(
//        elem.attributes
//            .collectFirst{ case scala.xml.PrefixedAttribute(_, id, scala.xml.Text(value), _) => value}
//            .get, CTLoader.obj[List[(Int, Int)]](elem
//                                                   .child
//                                                   .collectFirst{case c: scala.xml.Elem if c.prefix=="it" => c}
//                                                   .get))
    
    println(CTConverter.xml((1, 2))) // <- for some reason this is without typeParameters
    
}
