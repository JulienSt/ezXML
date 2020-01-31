package indv.jstengel.ezxml.extension.macros

import CTLoader.obj
import CTConverter.xml
import indv.jstengel.ezxml.extension.ClassLoadingTest.EmptyCaseClass
import indv.jstengel.ezxml.extension.ExtensionWrapper.{ElemWrapper, ObjWrapper}

object CTLoaderTest extends App {

    val i = xml(2)
    println(i)
    println(obj[Int](i))

    val a: Any = 23
    val a2 = xml(a)
    println(a2)
    println(obj[Int](a2))
    
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
    
    val i2: java.lang.Integer = 90
    println(i2 == obj[java.lang.Integer](i2.xml))
    println(i2 == obj[java.lang.Integer](xml(i2)))
    
    val s: java.lang.String = "TestInput"
    println(s == obj[java.lang.String](s.xml))
    println(s == obj[java.lang.String](xml(s)))
    
    println(EmptyCaseClass() == obj[EmptyCaseClass](xml(EmptyCaseClass())))
    
    // todo listen, Arrays, Maps and the other stuff that is included in CTConverter
    
}
