package jstengel.ezxml.core

import fastparse.NoWhitespace._
import fastparse._
import ElemParser._

import scala.language.implicitConversions

/**
 * this object is used for quick & dirty tests during development
 */
object QuickTest extends App {
    
    println(parse( """<Te:st val="blor" ></Te:st>""", ElemParser.Element(_)))
    
    println(parse("test:bla" , Name(_)))
    println(parse("testbla" , Name(_)))
    println(parse("'value'" , AttValue(_)))
    println(parse("\"value\"" , AttValue(_)))
    println(parse("test:bla='value'" , Attribute(_)))
    println(parse("""testbla = "value" """ , Attribute(_)))
    
    def headParser[_: P] = P( CharPred(_ != ':').rep.! ~ ":")
    def test[_: P]: P[Unit] = {
        headParser.flatMapX(head => P(CharPred(_ != ':').rep ~ ":" ~/ head))
    }
    
//    val parsedName = fastparse.parse( "Test", XmlParser.Name(_))
//    println(parsedName)
//    println(fastparse.parse( "Test", test(_)))
//    println(fastparse.parse( "Test", headParser(_)))
//    println(fastparse.parse( "Test:blbalabla:Test", test(_)))
//    println(fastparse.parse( "Test:blbalabla:Test", headParser(_)))
//    println(fastparse.parse( "Test:blbalabla:Bla", test(_)))
//    println(fastparse.parse( "Test:blbalabla:Bla", headParser(_)))

}