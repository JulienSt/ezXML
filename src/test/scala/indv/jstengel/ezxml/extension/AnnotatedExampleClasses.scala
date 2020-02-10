package indv.jstengel.ezxml.extension


import indv.jstengel.ezxml.extension.ct.{CTLoader, Xml}

import scala.xml.{Elem, PrefixedAttribute, Text}


object AnnotatedExampleClasses {
    
//    @Xml class SpecialTypeParameterTestClass[T1, T2, T3](a: T1, b: T2, c: T1, d: T3) {
//        val (a1, b1, c1, d1) = (a, b, c, d)
//        override def equals (obj : Any) : Boolean = obj match {
//            case other: SpecialTypeParameterTestClass[T1, T2, T3] =>
//                a == other.a1 &&
//                b == other.b1 &&
//                c == other.c1 &&
//                d == other.d1
//        }
//        override def toString : String = s"SpecialTypeParameterTestClass($a, $b, $c, $d)"
//    }
    
    @Xml class CTSpecialTypeParameterTestClass2[T1, T2](a: T2, b: T1, c: T1*) {
    
    }
    
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
        def contains(x: Int): Boolean =
            if (x< elem) left contains x
            else if (x > elem) right contains x
            else true
        
        def incl(x: Int): IntSet =
            if (x< elem) new NonEmpty(elem, left incl x, right)
            else if (x > elem) new NonEmpty(elem, left, right incl x)
            else this
        
        def union(other: IntSet): IntSet =
            ((left union right) union other) incl elem
        
        override def equals (obj : Any) : Boolean = obj.toString == toString
        
        override def toString : String = "{" + left + elem + right + "}"
    }
    
    @Xml class AnnotatedIntList(val i: Int*) extends Iterable[Int] {
        override def iterator : Iterator[Int] = i.iterator
        override def equals (obj : Any) : Boolean = obj match {
            case other: AnnotatedIntList => iterator.toList == other.iterator.toList
        }
    }
    
}
