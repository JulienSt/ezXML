package jstengel.ezxml.core

import org.scalatest.FlatSpec
import SimpleWrapper.NodeWrapper
import scala.xml.Elem

trait BasicChangeTest { this: FlatSpec =>
    val original : Elem =
        <bla>
            <aaa>
                <c/>
                <d data="test Data"/>
            </aaa>
            <bbb>
                <d data="test"/>
            </bbb>
        </bla>
    def testAdding(testableFunction : Elem => Elem, target : Elem): Unit = {
        /* test for strings, because internal definitions inside the xml-structure can lead to mismatches, even though
         * the same thing would have been printed to a hypothetical file.
         * since this library is mostly for easy saves and manipulations, the internal workings of the underlying
         * scala-xml-library are of no concern here */
        s"\n${ original.toPrettyXMLString } " should s" convert to ${ target.toPrettyXMLString } " in {
            assert(testableFunction(original).toPrettyXMLString == target.toPrettyXMLString)
        }
    }
}
