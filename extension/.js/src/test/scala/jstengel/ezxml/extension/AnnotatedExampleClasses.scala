package jstengel.ezxml.extension

import jstengel.ezxml.extension.ct.{CtDecoder, CtEncoder}
import jstengel.ezxml.extension.ct.Xml

import scala.reflect.ClassTag
import scala.xml.{Elem, PrefixedAttribute, Text}
import scala.reflect.runtime.universe.TypeTag

object AnnotatedExampleClasses {

// todo these two classes currently don't work due to the generics
//  generics and static annotations don't really mix well, therefor this topic is left open for a later release
//
//    class AnnotatedTypeParameterTestClass1[T1, T2, T3] (a : T1, b : T2, c : T1, d : T3)
//                                                       (implicit tt1: TypeTag[T1],
//                                                        ct1: ClassTag[T1],
//                                                        tt2: TypeTag[T2],
//                                                        ct2: ClassTag[T2],
//                                                        tt3: TypeTag[T3],
//                                                        ct3: ClassTag[T3])
//        extends jstengel.ezxml.extension.ct.XmlClassTrait {
//
//        val (a1, b1, c1, d1) = (a, b, c, d)
//
//        override def equals (obj : Any) : Boolean = obj match {
//            case other : AnnotatedTypeParameterTestClass1[T1, T2, T3] =>
//                a == other.a1 &&
//                b == other.b1 &&
//                c == other.c1 &&
//                d == other.d1
//        }
//        override def toString : String = s"AnnotatedTypeParameterTestClass1($a, $b, $c, $d)"
//        override def encode : Elem = CtEncoder.xmlMacro[AnnotatedTypeParameterTestClass1[T1, T2, T3]](this)
//    }
//
//    class CTSpecialTypeParameterTestClass2[T1, T2](a: T2, b: T1, c: T1*)(implicit tt1: TypeTag[T1],
//                                                                         ct1: ClassTag[T1],
//                                                                         tt2: TypeTag[T2],
//                                                                         ct2: ClassTag[T2])
//        extends jstengel.ezxml.extension.ct.XmlClassTrait {
//        val (a1, b1, c1) = (a, b, c)
//        override def equals (obj : Any) : Boolean = obj match {
//            case other : CTSpecialTypeParameterTestClass2[T1, T2] =>
//                a == other.a1 &&
//                b == other.b1 &&
//                c == other.c1
//        }
//        override def toString : String = s"CTSpecialTypeParameterTestClass2($a, $b, $c)"
//        override def encode : Elem = CtEncoder.xmlMacro[CTSpecialTypeParameterTestClass2[T1, T2]](this)
//    }
    
    trait TestTraitWithExtension { this: XmlClassTrait =>
        override def encode () : Elem = ???
        val a: String
    }
    object TestTraitWithExtension extends XmlObjectTrait {
        override def decode (elem : Elem) : TestTraitWithExtension with XmlClassTrait = {
            if(elem.label.contains(ApplyTestWithExtension.getClass.getName)){
                ApplyTestWithExtension.decode(elem)
            }
            else if(elem.label.contains(NonCaseRuntTimeTestWithExtension.getClass.getName)){
                ApplyTestWithExtension.decode(elem)
            }
            else
                ???
        }
        def unapply(elem: Elem): Option[_] = ???
        def unapply(elem: String): Option[_] = ???
    }

    //noinspection NotImplementedCode
    @Xml case class ApplyTestWithExtension (a : String, b : Int)
        extends TestTraitWithExtension {
        def apply(c: Double): Any = ???
    }

    @Xml class NonCaseRuntTimeTestWithExtension (val bla : Int, val blue : String, val im : ApplyTestWithExtension)
        extends TestTraitWithExtension {
        override val a : String = "test"
        override def equals (obj : Any) : Boolean = obj match {
            case nct: NonCaseRuntTimeTestWithExtension => bla == nct.bla && blue == nct.blue && im == nct.im
        }
    }
    
    @Xml class AnnotatedStrangeIterator(val id: String, it: List[(Int, Int)]) extends Iterable[(Int, Int)] {
        override val iterator : Iterator[(Int, Int)] = it.iterator
        override def equals (obj : Any) : Boolean = obj match {
            case other: AnnotatedStrangeIterator => iterator.toList == other.iterator.toList && id == other.id
        }
        override def toString : String = s"AnnotatedStrangeIterator($id, $it)"
    }

    @Xml class AnnotatedIntList(val i: Int*) extends Iterable[Int] {
        override def iterator : Iterator[Int] = i.iterator
        override def equals (obj : Any) : Boolean = obj match {
            case other: AnnotatedIntList => iterator.toList == other.iterator.toList
        }
    }

    @Xml class AnnotatedPrivateConstructorTest private (val a : Int*) {
        def this(b: Int) = this(b, b)
        override def equals (obj : Any) : Boolean = obj match {
            case other: AnnotatedPrivateConstructorTest => a == other.a
        }
        override def toString : String = s"PrivateConstructorTest(${a.mkString(", ")})"
    }

}
