package indv.jstengel.ezxml.core

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
                       <d data="heyhey"/>
                   </aaa>
                   <bbb>
                       <d data="du da"/>
                   </bbb>
                   <newChild></newChild>
               </bla>)
    testAdding(_.addChildren(<newChild></newChild>, <data/>),
               <bla>
                   <aaa>
                       <c/>
                       <d data="heyhey"/>
                   </aaa>
                   <bbb>
                       <d data="du da"/>
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
                       <d data="heyhey">
                           <newChild></newChild>
                       </d>
                   </aaa>
                   <bbb>
                       <d data="du da"/>
                   </bbb>
               </bla>)
}

/* test that the predicate is used correctly */
//@RunWith(classOf[JUnitRunner])
class PredicateAddTest extends FlatSpec with BasicChangeTest {
    testAdding(_ \~ "_" \~ ("d", _ \@ "data" == "du da") addChildren <newChild></newChild> get,
               <bla>
                   <aaa>
                       <c/>
                       <d data="heyhey"/>
                   </aaa>
                   <bbb>
                       <d data="du da">
                           <newChild></newChild>
                       </d>
                   </bbb>
               </bla>)
    testAdding(_ \\~ ("_", _ \@ "data" != "") addChildren <newChild></newChild> get,
               <bla>
                   <aaa>
                       <c/>
                       <d data="heyhey">
                           <newChild></newChild>
                       </d>
                   </aaa>
                   <bbb>
                       <d data="du da">
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
                                     <d data="heyhey">
                                         <newChild></newChild>
                                     </d>
                                 </aaa>
                                 <bbb>
                                     <d data="du da">
                                         <newChild></newChild>
                                     </d>
                                 </bbb>
                             </bla>)
}

/* test that multiple possible nodes lead to one correct addition add, when no predicate is given */
// todo more thorough testing of this case
@RunWith(classOf[JUnitRunner])
class MultipleChoiceAddTest extends FlatSpec with BasicChangeTest {
    override val original : Elem =
        <bla>
            <test/>
            <aaa>
                <c/>
                <d data="heyhey"/>
            </aaa>
            <bbb>
                <aaa data="du da"/>
                <test/>
            </bbb>
        </bla>
    testAdding(_ \~ "aaa" addChildren <newChild></newChild> get,
                 <bla>
                     <test/>
                     <aaa>
                         <c/>
                         <d data="heyhey"/>
                         <newChild></newChild>
                     </aaa>
                     <bbb>
                         <aaa data="du da"/>
                         <test/>
                     </bbb>
                 </bla>)
    testAdding(_ \~ "bbb" \~ "test" addChildren <newChild></newChild> get, // could be added to two <test/> nodes
                             <bla>
                                 <test/>
                                 <aaa>
                                     <c/>
                                     <d data="heyhey"/>
                                 </aaa>
                                 <bbb>
                                     <aaa data="du da"/>
                                     <test>
                                         <newChild></newChild>
                                     </test>
                                 </bbb>
                             </bla>)
}
