package indv.jstengel.ezxml.extension


import indv.jstengel.ezxml.extension.ct.Xml


object AnnotatedExampleClasses {
    
    //todo
    class StrangeIterator(id: String, iterator: List[(Int, Int)]) extends Iterator[(Int, Int)] {
        override def hasNext : Boolean = iterator.iterator.hasNext
        override def next () : (Int, Int) = iterator.iterator.next()
    }
    
    // todo
    case class ccIntList(i: Int*) extends Iterable[Int] {
        override def iterator : Iterator[Int] = i.iterator
    }
    
    // todo
    class IntList(val i: Int*) extends Iterable[Int] {
        override def iterator : Iterator[Int] = i.iterator
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
    
    //    @Xml class AnnotatedIntList(val i: Int*) extends Iterable[Int] {
    //        override def iterator : Iterator[Int] = i.iterator
    //    }
    
}
