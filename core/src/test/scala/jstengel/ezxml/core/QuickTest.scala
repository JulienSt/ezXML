package jstengel.ezxml.core

import SimpleWrapper.{ElemWrapper, NodeWrapper, NodeSeqWrapper}

import scala.xml.Elem

import scala.language.implicitConversions
import fastparse.NoWhitespace._
import fastparse._

/**
 * this object is used for quick & dirty tests during development
 */
object QuickTest extends App {
    
    def test[_: P]: P[String] = P( "@" ~ CharsWhile(_ != '@').rep.! ~ "@" ~ End )
    val parsed2 = fastparse.parse( "@test@", test(_))
    println(parsed2)
    
    
//    val parsed = fastparse.parse( "<Test></Test>", XmlParser.XmlExpr(_))
//    println(parsed)
    
}