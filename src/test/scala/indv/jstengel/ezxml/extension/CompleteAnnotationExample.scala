package indv.jstengel.ezxml.extension

import indv.jstengel.ezxml.extension.macros._


abstract class Bla {
    def a: String
}

@Xml
class CompleteAnnotationExample (@AsTextXML val s                        : String,
                                 @RuntimeXML val b                       : Bla,
                                 @SubstituteFieldXML("c") @RuntimeXML c1 : Int) {
    
    val c: Int = c1*2
    
    @CacheXML @SubstituteFieldXML("c") private val someHardCalculation = 5
    
    @RuntimeXML private lazy val privateStuff = 99
    @RuntimeXML lazy val publicStuff = 99
    @RuntimeXML private[this] lazy val extremelyPrivateStuff = 99
    
    /*
    private[FinalExample] var _someHardCalculationCache: Option[T] = None
    lazy val someHardCalculation = _someHardCalculationCache.getOrElse( das was hier vorher stand )
     */
    
}

object CompleteAnnotationExample {
    def get(fe: CompleteAnnotationExample) = fe.__someHardCalculationCache
}

// hier sollte dann automatisch ein companion-objekt erstellt werden, sofern noch keins existiert
// in dieses wird dann eine Ladefunktion zur Compile-Time erstellt
// dann sollte man demnach ein macro erstellen können, das dann auf diese Funktion verweist, anstatt zur runtime zu laden


// todo in tests alle versionen des Imports für die annotations testen. also:
//  app.xml.SubstituteXML
//  xml.SubstituteXML
//  SubstituteXML


// todo test schreiben, der test, dass nicht mehr als ein mal die gleiche annotation genutzt werden kann

object ExampleTest extends App {
    
    val fe = new CompleteAnnotationExample("miep", new Bla {
        override def a : String = "Bla-output"
    }, 1414)
    
    println("''''''''''")
    println(fe.xml.toPrettyXMLString)
    println(fe.publicStuff)
    println(CompleteAnnotationExample.get(fe))
    println("''''''''''")
    
}