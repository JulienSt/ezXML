package jstengel.ezxml.extension

import RTWrappers.{DecodingWrapper, EncodingWrapper}
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
    
    import jstengel.ezxml.core.SimpleWrapper.ElemWrapper
    
    class CurriedVarArgs[A, B](val a: A*)(val b: B*) {
        override def equals (obj : Any) : Boolean = obj match {
            case other: CurriedVarArgs[A, B] =>
                other.a.zip(a).forall(p => p._1 == p._2) && other.b.zip(b).forall(p => p._1 == p._2)
        }
    }
    
    val a = new CurriedVarArgs("String1", "test2", "String3", "test4")(1, 2, 3, 4, 5, 6)
    
    val encoded = a.xml // with this the object is automatically encoded into a xml element
    println(encoded.toPrettyXMLString)
    val decoded = encoded.obj[CurriedVarArgs[String, Int]].get // with this the xml object can be decoded
    println(a == decoded)
    
    val map =
        Map("jstengel.ezxml.extension.QuickTest.CurriedVarArgs[java.lang.String,scala.Int]" -> "Class[String,Int]",
            "a:scala.collection.immutable.Seq[java.lang.String]" -> "SeqA",
            "java.lang.String" -> "String",
            "b:scala.collection.immutable.Seq[java.lang.Integer]" -> "SeqB",
            "java.lang.Integer" -> "Int")
    val mapped = encoded.renameLabels(map.toSeq:_*)
    println(mapped.toPrettyXMLString)
    val reverseMap = map.map(_.swap)
    val decoded2 = mapped.renameLabels(reverseMap.toSeq:_*).obj[CurriedVarArgs[String, Int]].get
    println(a == decoded2)
    
    println(reverseMap.xml.toPrettyXMLString)
    println(reverseMap.xml.obj[Map[String, String]].get)
    
}
