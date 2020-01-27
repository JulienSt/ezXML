package indv.jstengel.ezxml.core

import indv.jstengel.ezxml.core.SimpleWrapper.{ElemWrapper, NodeWrapper}

import scala.xml.Elem

object PathTest extends App {
    
    val original : Elem =
        <bla>
            <slash><slash></slash></slash>
            <l0/>
            <aaa>
                <d data="test data"/>
            </aaa>
            <aaa></aaa>
            <l1/>
            <aaa>
                <c>
                    <x></x>
                </c>
                <d data="heyhey"/>
                <d data="extra"/>
            </aaa>
            <bbb>
                <aaa data="du da"/>
            </bbb>
            <ccc/>
            <ddd/>
        </bla>
    
//    println((ElemWrapper(original) \ "aaa").lesserParents.toPrettyXMLString)
//    println("~~~~~")
//    (ElemWrapper(original) \ "aaa").parentChildPairings.foreach(println)
//    println((ElemWrapper(original) \ "aaa" \ "c" \ "x").forwarders)
//    println("+++++++++++++++++++++++++++")
//    println((ElemWrapper(original) \ "aaa" \ "c").forwarders)
//    println("+++++++++++++++++++++++++++")
//    println((ElemWrapper(original) \ "aaa").parentChildPairings)
//    println(original \~ "aaa" \~ "d")
//    println((original \~ "aaa" \~ "d" addChildren <test/>).get.toPrettyXMLString)
//    println((original \~ "aaa" \~ "c" \~ "x" addChildren <test/>).get.toPrettyXMLString)
    println((original \\~ "aaa" addChildren (<test/>, <bla></bla>)).get.toPrettyXMLString)
//    println((original \\~ "aaa"))
//    original \\ "slash" foreach(println)
//    println(original \~ "aaa" addChildren <test/>)
//    println(original \~ "aaa")
//    println((ElemWrapper(original) \ "aaa" \ "c").connector match {
//                case Some(path: XmlPath) => path.targetElems
//                case Some(con: XmlPathList) => con.paths.head
//                case test => test
//            })
    
//    println(<bla><a></a><b></b></bla> == <bla><b></b><a></a></bla>)
}
