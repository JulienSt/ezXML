package jstengel.ezxml.core

import SimpleWrapper.ElemWrapper
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner

import scala.language.postfixOps
import scala.xml.Elem


/* test that the child is added to the correct path */
@RunWith(classOf[JUnitRunner])
class BasicAddTest extends FlatSpec with BasicChangeTest {
    testAdding(_.addChildren(<newChild></newChild>),
               <bla>
                   <aaa>
                       <c/>
                       <d data="test Data"/>
                   </aaa>
                   <bbb>
                       <d data="test"/>
                   </bbb>
                   <newChild></newChild>
               </bla>)
    testAdding(_.addChildren(<newChild></newChild>, <data/>),
               <bla>
                   <aaa>
                       <c/>
                       <d data="test Data"/>
                   </aaa>
                   <bbb>
                       <d data="test"/>
                   </bbb>
                   <newChild></newChild>
                   <data/>
               </bla>)
}

/* test that the child is added to the correct path */
@RunWith(classOf[JUnitRunner])
class BasicSubAddTest extends FlatSpec with BasicChangeTest {
    testAdding(_ \~ "aaa" \~ "d" addChildren <newChild></newChild> get,
               <bla>
                   <aaa>
                       <c/>
                       <d data="test Data">
                           <newChild></newChild>
                       </d>
                   </aaa>
                   <bbb>
                       <d data="test"/>
                   </bbb>
               </bla>)
}

/* test that the predicate is used correctly */
//@RunWith(classOf[JUnitRunner])
class PredicateAddTest extends FlatSpec with BasicChangeTest {
    testAdding(_ \~ "_" \~ ("d", _ \@ "data" == "test") addChildren <newChild></newChild> get,
               <bla>
                   <aaa>
                       <c/>
                       <d data="test Data"/>
                   </aaa>
                   <bbb>
                       <d data="test">
                           <newChild></newChild>
                       </d>
                   </bbb>
               </bla>)
    testAdding(_ \\~ ("_", _ \@ "data" != "") addChildren <newChild></newChild> get,
               <bla>
                   <aaa>
                       <c/>
                       <d data="test Data">
                           <newChild></newChild>
                       </d>
                   </aaa>
                   <bbb>
                       <d data="test">
                           <newChild></newChild>
                       </d>
                   </bbb>
               </bla>)
}

/* test that wrong definitions lead to None when trying to add a child */
@RunWith(classOf[JUnitRunner])
class WrongPredicateAddTest extends FlatSpec with BasicChangeTest {
    "\nTest " should " should return with None, because there is no such child " in {
        assert(original \~ "_" \~ ("d", _ \@ "data" == "nope") addChildren <newChild></newChild> isEmpty)
    }
    "\nTest2 " should " should return with None, because there is no such child " in {
        assert(original \\~ ("_", _ \@ "data" == "nope") addChildren <newChild></newChild> isEmpty)
    }
}

/* test that multiple correct paths lead to multiple add, when no predicate is given */
@RunWith(classOf[JUnitRunner])
class MultipleAddTest extends FlatSpec with BasicChangeTest {
    testAdding(_ \~ "_" \~ "d" addChildren <newChild></newChild> get,
                             <bla>
                                 <aaa>
                                     <c/>
                                     <d data="test Data">
                                         <newChild></newChild>
                                     </d>
                                 </aaa>
                                 <bbb>
                                     <d data="test">
                                         <newChild></newChild>
                                     </d>
                                 </bbb>
                             </bla>)
}

/* test that multiple possible nodes lead to one correct addition add, when no predicate is given */
@RunWith(classOf[JUnitRunner])
class MultipleChoiceAddTest extends FlatSpec with BasicChangeTest {
    override val original : Elem =
        <bla>
            <test/>
            <aaa>
                <c/>
                <d data="test Data"/>
            </aaa>
            <bbb>
                <aaa data="test"/>
                <test/>
            </bbb>
        </bla>
    testAdding(_ \~ "aaa" addChildren <newChild></newChild> get,
                 <bla>
                     <test/>
                     <aaa>
                         <c/>
                         <d data="test Data"/>
                         <newChild></newChild>
                     </aaa>
                     <bbb>
                         <aaa data="test"/>
                         <test/>
                     </bbb>
                 </bla>)
    testAdding(_ \~ "bbb" \~ "test" addChildren <newChild></newChild> get, // could be added to two <test/> nodes
                             <bla>
                                 <test/>
                                 <aaa>
                                     <c/>
                                     <d data="test Data"/>
                                 </aaa>
                                 <bbb>
                                     <aaa data="test"/>
                                     <test>
                                         <newChild></newChild>
                                     </test>
                                 </bbb>
                             </bla>)
}

/* test that changes are merged correctly */
@RunWith(classOf[JUnitRunner])
class MergeChangeTest extends FlatSpec with BasicChangeTest {
    override val original : Elem =
        <Test data="test1">
            <Test data="test21">
                <Test data="test31"/>
                <A data="testA1"/>
                <B data="testB1"/>
                <C data="testC1"/>
                <D data="testD1"/>
            </Test>
            <Test data="test22">
                <Test data="test32"/>
                <A data="testA2"/>
                <B data="testB2"/>
                <C data="testC2"/>
                <D data="testD2"/>
            </Test>
        </Test>
    testAdding(elem => {
                   (elem \\~ ("_", e => (e \@ "data").nonEmpty)).transformTarget(_.setAttribute("data", "value")) get
               },
               <Test data="test1">
                   <Test data="value">
                       <Test data="value"/>
                       <A data="value"/>
                       <B data="value"/>
                       <C data="value"/>
                       <D data="value"/>
                   </Test>
                   <Test data="value">
                       <Test data="value"/>
                       <A data="value"/>
                       <B data="value"/>
                       <C data="value"/>
                       <D data="value"/>
                   </Test>
               </Test>)
}
