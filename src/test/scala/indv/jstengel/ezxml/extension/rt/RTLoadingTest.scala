package indv.jstengel.ezxml.extension.rt

import indv.jstengel.ezxml.extension.AnnotatedExampleClasses._
import indv.jstengel.ezxml.extension.ExampleClasses
import indv.jstengel.ezxml.extension.ExampleClasses._
import indv.jstengel.ezxml.extension.ct.CTLoader.obj
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class RTLoadingTest extends FlatSpec with BasicRtLoadTest {
    val i: java.lang.Integer = 90
    test(i)
    
    val s: java.lang.String = "TestInput"
    test(s)
    
    test(EmptyCaseClass())
    test(None)
    test(ExampleClasses)
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
    test(CCWithMap(Map(2 -> CC1(5, "90ß"), 53 -> CC1(90, "965ß"))))
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
    
//    val tree = new NonEmpty(4,
//                            new NonEmpty(6,
//                                         new NonEmpty(19,
//                                                      EmptyIntSet,
//                                                      EmptyIntSet),
//                                         new NonEmpty(90,
//                                                      EmptyIntSet,
//                                                      EmptyIntSet)),
//                            new NonEmpty(13,
//                                         EmptyIntSet,
//                                         EmptyIntSet))
//    test(tree)
    
    test(ccNonIterIntList(1, 2, 3, 4, 5))
    test(new nonIterIntList(1, 2, 3, 4, 5, 6))
    
    test(new ClassWithArgs(1, 2, 3, 4, 5))
    test(new ClassWithArgs("a", "b", "c", "d", "e"))
    test(new ClassWithArgsAndExtra(1234, "a", "b", "c", "d", "e"))
    
    test(new CurriedClass(123)("test")(3.141592658))
    test(new CurriedClass2(123)("test")(3.141592658)(1.17))
    
    test(new CurriedVarArgs(1,2,3,4,5)("a", "b", "c", "d"))
    
    test(ccCurriedVarArgs(1,2,3,4,5)("a", "b", "c", "d"))
    
    val emptyList: List[Int] = List()
    test(emptyList)
    
    test(new IntList(1, 2, 3, 4, 5, 6))
    test(ccIntList(1, 2, 3, 4, 5, 6))
    
    val intTupleList: List[(Int, Int)] = List((1, 2), (2, 3), (3, 4))
    test(intTupleList)
    val tupleList: List[(Int, String)] = List((1, "s1"), (2, "s2"), (3, "s3"))
    test(tupleList)
    val emptyTupleList: List[(Int, String)] = List()
    test(emptyTupleList)
    
//    test(new AnnotatedIntList(1, 2, 3, 4, 5, 6))
    test(new StrangeIterator("testID", List((1, 2), (3, 4), (5, 6))))
    
    test(new AnnotatedStrangeIterator("testID", List((1, 2), (3, 4), (5, 6))))
    
    test(RTSpecialTypeParameterTestClass1(1, "test", 2, 4.56))
    test(RTSpecialTypeParameterTestClass2("test", 1, 1, 2, 3, 4, 5))
    
    test(new PrivateConstructorTest(5))
    
}