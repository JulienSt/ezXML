package indv.jstengel.ezxml.extension

import ClassLoadingTest._
//import core.SimpleWrapper.{ElemWrapper, NodeWrapper}
import ExtensionWrapper.{ElemWrapper, ObjWrapper}
import macros.Xml
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait BasicLoadTest { this: FlatSpec =>
    def test[A](a: A)(implicit tt : TypeTag[A], ct : ClassTag[A]): Unit = {
        val xml = a.xml
        s"\n$a as ${tt.tpe}" should s" load with $xml " in {assert(a == xml.obj[A].get)}
    }
    def testArray[A](a: Array[A])(implicit tt : TypeTag[Array[A]], ct : ClassTag[Array[A]]): Unit = {
        s"\n$a" should s" load with ${a.xml} " in {
            assert(a.xml.obj[Array[A]].get.zipWithIndex.forall(p => p._1 == a(p._2)))
        }
    }
}

@RunWith(classOf[JUnitRunner])
class ClassLoadingTestApp extends FlatSpec with BasicLoadTest {
    val i: java.lang.Integer = 90
    test(i)
    val s: java.lang.String = "TestInput"
    test(s)
    test(EmptyCaseClass())
    test(None)
    test(ClassLoadingTest)
    test(List("TestInput1", "TestInput2", "TestInput3"))
    test(List(1, 2, 3))
    test(Seq(1, 2, 3))
    test(Seq("TestInput1", "TestInput2"))
    test(CC1(5, "TestInput"))
    test(CC2(65, "TestInput"))
    test(NestedCC(CC2(65, "TestInput"),
                  "TestInput",
                  List(3, 7, 2, 6),
                  Array(CC2(4950495, "TestInput"),
                        CC2(495430935, "TestInput"))))
    test(NestedCC1("TestInput",
                   List(3, 7, 2, 6),
                   Array(1, 2, 3, 4)))
    test(NestedCC2("TestInput",
                   List(3, 7, 2, 6),
                   Array(List(1, 2), List(3, 4))))
    val eitherTest: Either[Int, List[List[Int]]] = Right(List(List(23, 52), List(1, 2, 3, 4)))
    test(eitherTest)
    test(Map(1 -> "TestInput1", 2 -> "TestInput2"))
    CCWithMap(Map(2 -> CC1(5, "90ß"), 53 -> CC1(90, "965ß")))
    test(new OptionTest(Some(34)))
    test(new ListClass(List(3, 4, 5, 6)))
    test(TypeParamTest(234))
    test(TypeParamTest2(234, "TestInput", List(1, 2, 3)))
    val applyTest : TestTrait = ApplyTest("bla", 123)
    test(applyTest)
    val nct: TestTrait = new NonCaseRuntTimeTest(12334, "lkjaslkjd", applyTest.asInstanceOf[ApplyTest])
    test(nct)
    val runtTimeList: List[TestTrait] = List(applyTest, nct)
    test(runtTimeList)
    private val bufferTest = mutable.Buffer(2, 3, 4)
    test(bufferTest)
    testArray(Array("heyhey", "blabla"))
    testArray(Array(2, 3, 4))
    val n: Option[Int] = None
    test(n)
    val someTest: Option[Int] = Some(4)
    test(someTest)
    test(Some(3, "5"))
    test(Tuple1(3))
    test(Tuple1(applyTest))
    test(Some(3))
    
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

    def getCorrectName(className: String, fieldName: String): Option[String] = {
        val FieldMapping = Map("scala.xml.Elem" -> Map("attributes1" -> "attributes"),
                               "scala.xml.Text" -> Map("data" -> "text"),
                               "scala.xml.UnprefixedAttribute" -> Map("next1" -> "next"))
        FieldMapping.get(className).flatMap(_.get(fieldName))
    }
    
    test(tree)
    
    test(ccNonIterIntList(1, 2, 3, 4, 5))
    test(new nonIterIntList(1, 2, 3, 4, 5, 6))
    
    test(new ClassWithArgs(1, 2, 3, 4, 5))
    test(new ClassWithArgs("a", "b", "c", "d", "e"))
    test(new ClassWithArgsAndExtra(1234, "a", "b", "c", "d", "e"))
    
    test(new CurriedClass(123)("test")(3.141592658))
    test(new CurriedClass2(123)("test")(3.141592658)(1.17))
    
    test(new CurriedVarArgs(1,2,3,4,5)("a", "b", "c", "d"))
    
    test(ccCurriedVarArgs(1,2,3,4,5)("a", "b", "c", "d"))
    
     // todo next
//    val treeXml = tree.xml
//    val treeXmlXml = treeXml.xmlWithMapping(getCorrectName)
//    s"\n${treeXml.toPrettyXMLString}" should s" load with $treeXmlXml " in {
//        assert(treeXml == treeXmlXml.obj[scala.xml.Elem].get)
//    }


// todo ab hier failures durchtesten und auf korrekten error matchen

//    println(List(2, 3, 4).xml.obj[List[String]])
//    val i2 = 2
//    println(i2.xml)
//    println(i2.xml.obj)
}

object quicktest extends App {
    
//    import scala.reflect.{runtime => rt}
//    import rt.{universe => ru}
//    import ru._
//    private implicit val rm: Mirror = rt.currentMirror
    
    
    
    // todo entweder annotation anpassen oder irgendwie rausfiltern/herausfinden, dass es varargs gibt und dann so speichern
//    println(new IntList(1, 2, 3, 4, 5, 6).xml.toPrettyXMLString)
//    println(new IntList(1, 2, 3, 4, 5, 6).xml.obj[IntList])
//    println(new AnnotatedIntList(1, 2, 3, 4, 5, 6).xml.toPrettyXMLString)
//    println(new AnnotatedIntList(1, 2, 3, 4, 5, 6).xml.obj[AnnotatedIntList])
//    println(ccIntList(1, 2, 3, 4, 5, 6).xml.toPrettyXMLString)
//    println(ccIntList(1, 2, 3, 4, 5, 6).xml.obj[ccIntList])
//    println(new NonEmpty())
}

//object VarArgsTest extends App {
//    import scala.reflect.{runtime => rt}
//    import rt.{universe => ru}
//    import ru._
//    private implicit val rm: Mirror = rt.currentMirror
//
//    case class ccCurriedVarArgs[A, B](a: A*)(b: B*)
//    val ccVarArgsTest = ccCurriedVarArgs(1,2,3,4,5)("a", "b", "c", "d")
//
//    val reflectedObj  = rm.reflect(ccVarArgsTest)
////    println(util.Try(ccVarArgsTest.b)) <-- does not work, because b is not made a public member unlike in example 2
//    println(reflectedObj.symbol.typeSignature)
//
//    case class ccCurriedVarArgs2[A, B](a: A*)(val b: B*)
//    val ccVarArgsTest2 = ccCurriedVarArgs2(1,2,3,4,5)("a", "b", "c", "d")
//
//    val reflectedObj2  = rm.reflect(ccVarArgsTest2)
//    println(ccVarArgsTest2.b)
//    println(reflectedObj2.symbol.typeSignature)
//}


object ClassLoadingTest {
    
    
    trait TestTrait {
        val a: String
    }
    case class ApplyTest (a : String, b : Int) extends TestTrait {
        def apply(c: Double): Any = ???
    }
    class NonCaseRuntTimeTest (val bla : Int, val blue : String, val im : ApplyTest) extends TestTrait {
        override val a : String = "blabla"
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
    }
    class IntList(val i: Int*) extends Iterable[Int] {
        override def iterator : Iterator[Int] = i.iterator
    }
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
    
    
    //todo fehlt
    class StrangeIterator(id: String, iterator: List[(Int, Int)]) extends Iterator[(Int, Int)] {
        override def hasNext : Boolean = iterator.iterator.hasNext
        override def next () : (Int, Int) = iterator.iterator.next()
    }
    
    // todo einmal mit einer Menge von Array[Int] testen
    
    case class ccIntList(i: Int*) extends Iterable[Int] {
        override def iterator : Iterator[Int] = i.iterator
    }
    
    
}