package jstengel.ezxml.extension

import jstengel.ezxml.extension.ct.{CtDecoder, CtEncoder}
import jstengel.ezxml.extension.ct.Xml

import scala.reflect.ClassTag
import scala.xml.{Elem, PrefixedAttribute, Text}
import scala.reflect.runtime.universe.TypeTag

object AnnotatedExampleClasses {
    
    
//    class AnnotatedTypeParameterTestClass1[T1, T2, T3] (a : T1, b : T2, c : T1, d : T3)
//                                                       (implicit tt1: TypeTag[T1],
//                                                        ct1: ClassTag[T1],
//                                                        tt2: TypeTag[T2],
//                                                        ct2: ClassTag[T2],
//                                                        tt3: TypeTag[T3],
//                                                        ct3: ClassTag[T3]) extends jstengel.ezxml.extension.XmlClassTrait {
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
//        override def saveAsXml : Elem = CtEncoder.xmlMacro[AnnotatedTypeParameterTestClass1[T1, T2, T3]](this)
//    }
//
//    class CTSpecialTypeParameterTestClass2[T1, T2](a: T2, b: T1, c: T1*) {
//
//    }
    
    @Xml class AnnotatedStrangeIterator(val id: String, it: List[(Int, Int)]) extends Iterable[(Int, Int)] {
        override val iterator : Iterator[(Int, Int)] = it.iterator
        override def equals (obj : Any) : Boolean = obj match {
            case other: AnnotatedStrangeIterator => iterator.toList == other.iterator.toList && id == other.id
        }
        override def toString : String = s"AnnotatedStrangeIterator($id, $it)"
    }

    trait IntSet {
        def incl(x: Int): IntSet
        def contains(x: Int): Boolean
        def union(other: IntSet): IntSet
    }
    object EmptyIntSet extends IntSet {
        def contains(x: Int): Boolean = false
        def incl(x: Int): IntSet = new NonEmpty(x, EmptyIntSet, EmptyIntSet)
        def union(other: IntSet): IntSet = other
        override def equals (obj : Any) : Boolean = obj == EmptyIntSet
        override def toString = "."
    }
    @Xml class NonEmpty(elem : Int, left : IntSet, right : IntSet) extends IntSet {

        def contains (x : Int) : Boolean =
            if ( x < elem ) left contains x
            else if ( x > elem ) right contains x
            else true

        def incl (x : Int) : IntSet =
            if ( x < elem ) new NonEmpty(elem, left incl x, right)
            else if ( x > elem ) new NonEmpty(elem, left, right incl x)
            else this

        def union (other : IntSet) : IntSet =
            ( ( left union right ) union other ) incl elem

        override def equals (obj : Any) : Boolean = obj.toString == toString

        override def toString : String = "{" + left + elem + right + "}"

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
