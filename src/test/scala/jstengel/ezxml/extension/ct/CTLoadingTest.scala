package jstengel.ezxml.extension.ct

import CtDecoder.obj
import CtEncoder.{xml, xmlMacro}
import jstengel.ezxml.extension.AnnotatedExampleClasses.AnnotatedStrangeIterator
import jstengel.ezxml.extension.AnnotatedExampleClasses.{AnnotatedIntList, AnnotatedStrangeIterator, EmptyIntSet, IntSet, NonEmpty}
import jstengel.ezxml.extension.ExampleClasses
import jstengel.ezxml.extension.ExampleClasses._
import jstengel.ezxml.extension.RTWrappers.{ElemWrapper, ObjWrapper}
import jstengel.ezxml.extension.AnnotatedExampleClasses.{AnnotatedIntList, AnnotatedStrangeIterator, EmptyIntSet, IntSet}
import jstengel.ezxml.extension.ExampleClasses
import jstengel.ezxml.extension.ExampleClasses.{ApplyTest, CC1, CC2, CCWithMap, ClassWithArgs, ClassWithArgsAndExtra, CurriedClass, CurriedClass2, CurriedVarArgs, EmptyCaseClass, IntList, ListClass, NestedCC, NestedCC1, NestedCC2, PrivateConstructorTest, RTSpecialTypeParameterTestClass1, RTSpecialTypeParameterTestClass2, StrangeIterator, TestTrait, TypeParamTest, TypeParamTest2, ccCurriedVarArgs, ccIntList, ccNonIterIntList, nonIterIntList}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable
import scala.xml.Elem

@RunWith(classOf[JUnitRunner])
class CTLoadingTest extends FlatSpec {
    
    def testArray[A](orig: Array[A], f: Array[A] => Elem, g: Elem => Array[A]) : Unit = {
        val elem = f(orig)
        val loaded = g(elem)
        s"\n$orig" should s" load with $elem " in {
            assert(orig.sameElements(loaded))
        }
    }

    def test[A](orig: A, f: A => Elem, g: Elem => A): Unit = {
        val elem = f(orig)
        val loaded = g(elem)
        s"\n$orig" should s" load with $elem, $f, and $g" in {
            assert(orig == loaded)
        }
    }

    def test[A](orig: A, elem: Elem, g: Elem => A): Unit = {
        val loaded = g(elem)
        s"\n$orig" should s" load with $elem and $g" in {
            assert(orig == loaded)
        }
    }

    val i: java.lang.Integer = 90
    test(i, xmlMacro[java.lang.Integer], obj[java.lang.Integer])

    val s: java.lang.String = "TestInput"
    test(s, xmlMacro[java.lang.String], obj[java.lang.String])

    test(EmptyCaseClass(), xmlMacro[EmptyCaseClass], obj[EmptyCaseClass])
    test(None, xmlMacro[None.type], obj[None.type])
    test(ExampleClasses, xmlMacro[ExampleClasses.type], obj[ExampleClasses.type])
    test(List("TestInput1", "TestInput2", "TestInput3"), xmlMacro[List[String]], obj[List[String]])
    test(List(1, 2, 3), xmlMacro[List[Int]], obj[List[Int]])
    test(Seq(1, 2, 3), xmlMacro[Seq[Int]], obj[Seq[Int]])
    test(Seq("TestInput1", "TestInput2"), xmlMacro[Seq[String]], obj[Seq[String]])
    test(CC1(5, "TestInput"), xmlMacro[CC1], obj[CC1])
    test(CC2(65, "TestInput"), xmlMacro[CC2], obj[CC2])
    test(NestedCC(CC2(65, "TestInput"),
                  "TestInput",
                  List(3, 7, 2, 6),
                  Array(CC2(4950495, "TestInput"),
                        CC2(495430935, "TestInput"))), xmlMacro[NestedCC], obj[NestedCC])
    test(NestedCC1("TestInput",
                   List(3, 7, 2, 6),
                   Array(1, 2, 3, 4)), xmlMacro[NestedCC1], obj[NestedCC1])
    test(NestedCC2("TestInput",
                   List(3, 7, 2, 6),
                   Array(List(1, 2), List(3, 4))), xmlMacro[NestedCC2], obj[NestedCC2])

    val eitherTest: Either[Int, List[List[Int]]] = Right(List(List(23, 52), List(1, 2, 3, 4)))
    test(eitherTest, xmlMacro[Either[Int, List[List[Int]]]], obj[Either[Int, List[List[Int]]]])

    test(Map(1 -> "TestInput1", 2 -> "TestInput2"), xmlMacro[Map[Int, String]], obj[Map[Int, String]])
    test(CCWithMap(Map(2 -> CC1(5, "90ß"), 53 -> CC1(90, "965ß"))), xmlMacro[CCWithMap], obj[CCWithMap])

    test(new OptionTest(Some(34)), xmlMacro[OptionTest], obj[OptionTest])
    test(new ListClass(List(3, 4, 5, 6)), xmlMacro[ListClass], obj[ListClass])
    test(TypeParamTest(234), xmlMacro[TypeParamTest[Int]], obj[TypeParamTest[Int]])
    test(TypeParamTest2(234, "TestInput", List(1, 2, 3)),
         xmlMacro[TypeParamTest2[Int, String, List[Int]]], obj[TypeParamTest2[Int, String, List[Int]]])
    val applyTest : TestTrait = ApplyTest("bla", 123)
    test(applyTest, xmlMacro[TestTrait], obj[TestTrait])
    val nct: TestTrait = new NonCaseRuntTimeTest(12334, "lkjaslkjd", applyTest.asInstanceOf[ApplyTest])
    test(nct, xmlMacro[TestTrait], obj[TestTrait])

    val runtTimeList: List[TestTrait] = List(applyTest, nct)
    test(runtTimeList, xmlMacro[List[TestTrait]], obj[List[TestTrait]])

    private val bufferTest = mutable.Buffer(2, 3, 4)
    test(bufferTest, xmlMacro[mutable.Buffer[Int]], obj[mutable.ArrayBuffer[Int]])
    // note: for some reason obj[mutable.Buffer[Int]] does not work (it doesn't even compile)

    testArray(Array("heyhey", "blabla"), xmlMacro[Array[String]], obj[Array[String]])
    testArray(Array(2, 3, 4), xmlMacro[Array[Int]], obj[Array[Int]])
    val n: Option[Int] = None
    test(n, xmlMacro[Option[Int]], obj[Option[Int]])
    val someTest: Option[Int] = Some(4)
    test(someTest, xmlMacro[Option[Int]], obj[Option[Int]])
    test(Some(3, "5"): Option[(Int, String)], xmlMacro[Option[(Int, String)]], obj[Option[(Int, String)]])
    test(Tuple1(3), xmlMacro[Tuple1[Int]], obj[Tuple1[Int]])
    test(Tuple1(applyTest), xmlMacro[Tuple1[TestTrait]], obj[Tuple1[TestTrait]])
    test(Some(3): Option[Int], xmlMacro[Option[Int]], obj[Option[Int]])

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
    test(tree, xmlMacro[IntSet], obj[IntSet])

    test(ccNonIterIntList(1, 2, 3, 4, 5), xmlMacro[ccNonIterIntList], obj[ccNonIterIntList])
    test(new nonIterIntList(1, 2, 3, 4, 5, 6), xmlMacro[nonIterIntList], obj[nonIterIntList])

    test(new ClassWithArgs(1, 2, 3, 4, 5), xmlMacro[ClassWithArgs[Int]], obj[ClassWithArgs[Int]])
    test(new ClassWithArgs("a", "b", "c", "d", "e"), xmlMacro[ClassWithArgs[String]], obj[ClassWithArgs[String]])
    test(new ClassWithArgsAndExtra(1234, "a", "b", "c", "d", "e"),
         xmlMacro[ClassWithArgsAndExtra[String]], obj[ClassWithArgsAndExtra[String]])

    test(new CurriedClass(123)("test")(3.141592658), xmlMacro[CurriedClass], obj[CurriedClass])
    test(new CurriedClass2(123)("test")(3.141592658)(1.17), xmlMacro[CurriedClass2], obj[CurriedClass2])

    test(new CurriedVarArgs(1,2,3,4,5)("a", "b", "c", "d"),
         xmlMacro[CurriedVarArgs[Int, String]], obj[CurriedVarArgs[Int, String]])

    test(ccCurriedVarArgs(1,2,3,4,5)("a", "b", "c", "d"),
         xmlMacro[ccCurriedVarArgs[Int, String]], obj[ccCurriedVarArgs[Int, String]])

    val emptyList: List[Int] = List()
    test(emptyList, xmlMacro[List[Int]], obj[List[Int]])

    test(new IntList(1, 2, 3, 4, 5, 6), xmlMacro[IntList], obj[IntList])
    test(ccIntList(1, 2, 3, 4, 5, 6), xmlMacro[ccIntList], obj[ccIntList])

    val intTupleList: List[(Int, Int)] = List((1, 2), (2, 3), (3, 4))
    test(intTupleList, xmlMacro[List[(Int, Int)]], obj[List[(Int, Int)]])
    val tupleList: List[(Int, String)] = List((1, "s1"), (2, "s2"), (3, "s3"))
    test(tupleList, xmlMacro[List[(Int, String)]], obj[List[(Int, String)]])
    val emptyTupleList: List[(Int, String)] = List()
    test(emptyTupleList, xmlMacro[List[(Int, String)]], obj[List[(Int, String)]])

    test(new AnnotatedIntList(1, 2, 3, 4, 5, 6), xmlMacro[AnnotatedIntList], obj[AnnotatedIntList])
    test(new StrangeIterator("testID", List((1, 2), (3, 4), (5, 6))), xmlMacro[StrangeIterator], obj[StrangeIterator])

    val annotatedIt = new AnnotatedStrangeIterator("testID", List((1, 2), (3, 4), (5, 6)))
    test(annotatedIt, xml(annotatedIt), obj[AnnotatedStrangeIterator])

    test(RTSpecialTypeParameterTestClass1(1, "test", 2, 4.56),
         xmlMacro[RTSpecialTypeParameterTestClass1[Int, String, Double]],
         obj[RTSpecialTypeParameterTestClass1[Int, String, Double]])
    test(RTSpecialTypeParameterTestClass2("test", 1, 1, 2, 3, 4, 5),
         xmlMacro[RTSpecialTypeParameterTestClass2[Int, String]], obj[RTSpecialTypeParameterTestClass2[Int, String]])

    test(new PrivateConstructorTest(5), xmlMacro[PrivateConstructorTest], obj[PrivateConstructorTest])

}
