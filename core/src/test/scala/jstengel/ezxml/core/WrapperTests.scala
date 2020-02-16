package jstengel.ezxml.core

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import SimpleWrapper._

import scala.xml.Elem

@RunWith(classOf[JUnitRunner])
class WrapperTests extends FlatSpec {
    val elem: Elem = <A/>
    
    def test(original : Elem, target : Elem, conversionFunction: Elem => Elem): Unit = {
        s"\n${ original.toPrettyXMLString } " should s" convert to ${ target.toPrettyXMLString } " in {
            /* test for strings, because internal definitions inside the xml-structure can lead to mismatches, even though
             * the same thing would have been printed to a hypothetical file.
             * since this library is mostly for easy saves and manipulations, the internal workings of the underlying
             * scala-xml-library are of no concern here */
            assert(conversionFunction(original).toPrettyXMLString == target.toPrettyXMLString)
        }
    }
    
    test(elem, <A test="1"/>, _.setAttribute("test", "1"))
    test(elem, <A test1="1" test2="2"/>, _.setAttributes(("test1", "1"), ("test2", "2")))
    test(elem, <A pre1:test1="1"/>, _.setAttWithPre("pre1", "test1", "1"))
    test(elem, <A pre1:test1="1" pre2:test2="2"/>, _.setAttsWithPre(("pre1", "test1", "1"), ("pre2", "test2", "2")))
    test(elem, <A pre1:test1="1" test1="1"/>, _.setAttribute("test1", "1").setAttWithPre("pre1", "test1", "1"))
    
    val elem2: Elem = <A><B/><C/><D/></A>
    
    test(elem2, <A/>, _.root)
    test(elem2,
         <A><B/></A>,
         _.filterChildren(_.label == "B"))
    test(elem2,
         <A><C/><D/></A>,
         _.deleteChildren(_.label == "B"))
    test(elem2,
         <A><B/><C/><D/><E/><F/></A>,
         _.addChildren(<E/>, <F/>))
    test(elem2,
         <A>
             <B testKey="testValue"/>
             <C testKey="testValue"/>
             <D testKey="testValue"/>
         </A>,
         _.mapChildren{case e: Elem => e.setAttribute("testKey", "testValue") case n => n})
    test(elem2,
         <A>
             <B testKey1="testValue1"/>
             <B testKey2="testValue2"/>
             <C testKey1="testValue1"/>
             <C testKey2="testValue2"/>
             <D testKey1="testValue1"/>
             <D testKey2="testValue2"/>
         </A>,
         _.flatMapChildren{
             case e: Elem => List(e.setAttribute("testKey1", "testValue1"),
                                  e.setAttribute("testKey2", "testValue2"))
             case n => n
         })
    test(elem2,
         <A testKey="testValue">
             <B/>
             <C/>
             <D/>
         </A>,
         _.transformRoot{_.setAttribute("testKey", "testValue")})
    
    test(<Test data="test1">
             <pre1:Test data="test21">
                 <Test data="test31"/>
                 <preA:A data="testA1"/>
                 <B data="testB1"/>
                 <C data="testC1"/>
                 <D data="testD1"/>
             </pre1:Test>
             <pre2:Test data="test22">
                 <Test data="test32"/>
                 <preA:A data="testA2"/>
                 <pre:B data="testB2"/>
                 <C data="testC2"/>
                 <preD:D data="testD2"/>
             </pre2:Test>
         </Test>,
         <Test data="test1">
             <PreTest data="test21">
                 <Test data="test31"/>
                 <prefix:LabelA data="testA1"/>
                 <B data="testB1"/>
                 <preC:C data="testC1"/>
                 <NewDLabel data="testD1"/>
             </PreTest>
             <PreTest data="test22">
                 <Test data="test32"/>
                 <prefix:LabelA data="testA2"/>
                 <pre:B data="testB2"/>
                 <preC:C data="testC2"/>
                 <NewDLabel data="testD2"/>
             </PreTest>
         </Test>,
         _.renameLabels("pre1:Test" -> "PreTest",
                        "pre2:Test" -> "PreTest",
                        "preA:A" -> "prefix:LabelA",
                        "C" -> "preC:C",
                        "_:D" -> "NewDLabel")
         )
        
    test(<Test pre:data="test1">
             <Test pre:dataT="test21">
                 <Test pre:dataT="test31"/>
                 <A pre:data="testA1"/>
                 <B pre:data="testB1"/>
                 <C dataC="testC1"/>
                 <D pre:data="testD1"/>
             </Test>
             <Test preT:dataT="test22">
                 <Test preT:dataT="test32"/>
                 <A data="testA2"/>
                 <B preB:dataB="testB2"/>
                 <C dataC="testC2"/>
                 <D preD:dataD="testD2"/>
             </Test>
         </Test>,
         <Test preDataTest="test1">
             <Test newDataT="test21">
                 <Test newDataT="test31"/>
                 <A preDataTest="testA1"/>
                 <B preDataTest="testB1"/>
                 <C preC:dataCc="testC1"/>
                 <D preDataTest="testD1"/>
             </Test>
             <Test newDataT="test22">
                 <Test newDataT="test32"/>
                 <A testData="testA2"/>
                 <B prefix:dataForB="testB2"/>
                 <C preC:dataCc="testC2"/>
                 <D preD:dataD="testD2"/>
             </Test>
         </Test>,
         _.renameAttributes("pre:data" -> "preDataTest",
                            "data" -> "testData",
                            "preB:dataB" -> "prefix:dataForB",
                            "dataC" -> "preC:dataCc",
                            "_:dataT" -> "newDataT")
         )
    
}

/* Prints for WrapperTests */
object WrapperTestExamples extends App {
    val elem: Elem = <A/>
    println(elem.setAttribute("test", "1").toPrettyXMLString)
    println(elem.setAttributes(("test1", "1"), ("test2", "2")).toPrettyXMLString)
    println(elem.setAttWithPre("pre1", "test1", "1").toPrettyXMLString)
    println(elem.setAttsWithPre(("pre1", "test1", "1"), ("pre2", "test2", "2")).toPrettyXMLString)
    println(elem.setAttribute("test1", "1").setAttWithPre("pre1", "test1", "1").toPrettyXMLString)
    
    val elem2: Elem = <A><B/><C/><D/></A>
    println(elem2.root)
    
    println(elem2.filter(_.label == "B"))
    println(elem2.deleteChildren(_.label == "B"))
    println(elem2.addChildren(<E/>, <F/>))
    println(elem2.mapChildren{case e: Elem => e.setAttribute("testKey", "testValue") case n => n})
    println(elem2.flatMapChildren{case e: Elem => List(e.setAttribute("testKey1", "testValue1"),
                                                       e.setAttribute("testKey2", "testValue2")) case n => n})
    println(elem2.transformRoot{_.setAttribute("testKey", "testValue")})
    
    val renaming =
        <Test data="test1">
            <pre1:Test data="test21">
                <Test data="test31"/>
                <preA:A data="testA1"/>
                <B data="testB1"/>
                <C data="testC1"/>
                <D data="testD1"/>
            </pre1:Test>
            <pre2:Test data="test22">
                <Test data="test32"/>
                <preA:A data="testA2"/>
                <pre:B data="testB2"/>
                <C data="testC2"/>
                <preD:D data="testD2"/>
            </pre2:Test>
        </Test>
    
    println(renaming.renameLabels("pre1:Test" -> "PreTest",
                                  "pre2:Test" -> "PreTest",
                                  "preA:A" -> "prefix:LabelA",
                                  "C" -> "preC:C",
                                  "_:D" -> "NewDLabel").toPrettyXMLString)
}
