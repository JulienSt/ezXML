package indv.jstengel.ezxml.extension

object ExampleClasses {
    
    trait TestTrait {
        val a: String
    }
    
    //noinspection NotImplementedCode
    case class ApplyTest (a : String, b : Int) extends TestTrait {
        def apply(c: Double): Any = ???
    }
    
    class NonCaseRuntTimeTest (val bla : Int, val blue : String, val im : ApplyTest) extends TestTrait {
        override val a : String = "test"
        override def equals (obj : Any) : Boolean = obj match {
            case nct: NonCaseRuntTimeTest => bla == nct.bla && blue == nct.blue && im == nct.im
        }
    }
    
    class ListClass(val l: List[Int]) {
        override def toString : String = s"ListClass($l)"
        override def equals (obj : Any) : Boolean = obj match {
            case lc: ListClass => l == lc.l
        }
    }
    
    case class TypeParamTest[A](a: A)
    case class TypeParamTest2[A, B, C](a: A, b: B, c: C)
    
    class OptionTest(val o: Option[Int]){
        override def toString : String = s"OpTest($o)"
        override def equals (obj : Any) : Boolean = obj match {
            case ot: OptionTest => o == ot.o
        }
    }
    
    case class CC1 (i : Int, s : String)
    case class CC2 (i : Int, s : String)
    
    case class NestedCC (b : CC2, s : String, l : List[Int], a : Array[CC2]) {
        override def equals (obj : Any) : Boolean = obj match {
            case NestedCC(b1, s1, l1, a1) => b == b1 && s == s1 && l == l1 && a.zip(a1).forall(p => p._1 == p._2)
        }
    }
    case class NestedCC1 (s : String, l : List[Int], a : Array[Int]) {
        override def equals (obj : Any) : Boolean = obj match {
            case NestedCC1(s1, l1, a1) => s == s1 && l == l1 && a.zip(a1).forall(p => p._1 == p._2)
        }
    }
    case class NestedCC2 (s : String, l : List[Int], a : Array[List[Int]]) {
        override def equals (obj : Any) : Boolean = obj match {
            case NestedCC2(s1, l1, a1) => s == s1 && l == l1 && a.zip(a1).forall(p => p._1 == p._2)
        }
    }
    
    case class CCWithMap (m: Map[Int, CC1])
    case class EmptyCaseClass ()
    
    
    class nonIterIntList(val i: Int*) {
        override def equals (obj : Any) : Boolean = obj match {
            case other: nonIterIntList => other.i.zip(i).forall(p => p._1 == p._2)
        }
    }
    
    case class ccNonIterIntList(i: Int*) {
        override def equals (obj : Any) : Boolean = obj match {
            case other: ccNonIterIntList => other.i.zip(i).forall(p => p._1 == p._2)
        }
    }
    
    class ClassWithArgs[A](val a: A*) {
        override def equals (obj : Any) : Boolean = obj match {
            case other: ClassWithArgs[A] => other.a.zip(a).forall(p => p._1 == p._2)
        }
    }
    class ClassWithArgsAndExtra[A](val num : Int, val a: A*) {
        override def equals (obj : Any) : Boolean = obj match {
            case other: ClassWithArgsAndExtra[A] => num == other.num && other.a.zip(a).forall(p => p._1 == p._2)
        }
    }
    
    class CurriedClass(val a: Int)(val b: String)(val c: Double) {
        override def equals (obj : Any) : Boolean = obj match {
            case other: CurriedClass => a == other.a && b == other.b && c == other.c
        }
    }
    class CurriedClass2(val a: Int)(val b: String)(val c: Double)(val d: Double){
        override def equals (obj : Any) : Boolean = obj match {
            case other: CurriedClass2 => a == other.a && b == other.b && c == other.c && d == other.d
        }
    }
    
    class CurriedVarArgs[A, B](val a: A*)(val b: B*) {
        override def equals (obj : Any) : Boolean = obj match {
            case other: CurriedVarArgs[A, B] =>
                other.a.zip(a).forall(p => p._1 == p._2) && other.b.zip(b).forall(p => p._1 == p._2)
        }
    }
    
    // case class ccCurriedVarArgs[A, B](a: A*)(b: B*) // <- this does not work, as b is not automatically
    // transformed to a public member
    case class ccCurriedVarArgs[A, B](a: A*)(val b: B*) // has to be with a val or var to be accessible
    
}
