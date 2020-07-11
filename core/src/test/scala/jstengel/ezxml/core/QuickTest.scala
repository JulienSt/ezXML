package jstengel.ezxml.core

import fastparse.NoWhitespace._
import fastparse._

import scala.language.implicitConversions

/**
 * this object is used for quick & dirty tests during development
 */
object QuickTest extends App {
    
//    def test[_: P]: P[String] =
//        P( "@" ~
//           (!"@" ~ SingleChar.rep.flatMap{
//               case 's' => 'a'
//               case s => s
//           }).!
//           ~ "@" ~ End
//        )
//    val parsed2 = fastparse.parse( "@test@", test(_))
//    println(parsed2)
    
    def stringChars(c: Char) = c != '\"' && c != '\\'
    
    def space[_: P]: P0         = P( CharsWhileIn(" \r\n", 0) )
    def digits[_: P]: P0        = P( CharsWhileIn("0-9") )
    def exponent[_: P]      = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
    def fractional[_: P]    = P( "." ~ digits )
    def integral[_: P]      = P( "0" | CharIn("1-9")  ~ digits.? )
    
    def number[_: P] = P(  CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
        x => Num(x.toDouble)
        )
    
    def `false`[_: P]       = P( "false" ).map(_ => False)
    def `true`[_: P]        = P( "true" ).map(_ => True)
    def `null`[_: P]        = P( "null" ).map(_ => True)
    
    def hexDigit[_: P]      = P( CharIn("0-9a-fA-F") )
    def unicodeEscape[_: P] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
    def escape[_: P]        = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )
    
    def strChars[_: P] = P( CharsWhile(stringChars) )
    def string[_: P] =
        P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Str)
    
    def array[_: P] =
        P( "[" ~/ jsonExpr.rep(sep=","./) ~ space ~ "]").map(Arr(_:_*))
    
    def pair[_: P] = P( string.map(_.value) ~/ ":" ~/ jsonExpr )
    
    def obj[_: P] =
        P( "{" ~/ pair.rep(sep=","./) ~ space ~ "}").map(Obj(_:_*))
    
    def jsonExpr[_: P]: P[Val] = P(
        space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
        )
    
    val parsed = fastparse.parse( "<Test></Test>", XmlParser.XmlExpr(_))
    println(parsed)


    val input = """{
                  |  "firstName": "John",
                  |  "lastName": "Smith",
                  |  "age": 25,
                  |  "address": {
                  |      "streetAddress": "21 2nd Street",
                  |      "city": "New York",
                  |      "state": "NY",
                  |      "postalCode": 10021
                  |  },
                  |  "phoneNumbers": [
                  |      {
                  |          "type": "home",
                  |          "number": "212 555-1234"
                  |      },
                  |      {
                  |          "type": "fax",
                  |          "number": "646 555-4567"
                  |      }
                  |  ]
                  |}""".stripMargin
    
//    val Parsed.Success(value, _) = parse(input, jsonExpr(_))
//    println(value)
//
//    println("test:bla".span(_ != ':'))
    
//    def Name[_: P]: P[(String, String)] = P( (NameStart ~ NameChar.rep).! ~ ":".? ~ NameChar.rep.! )
//    println(parse("test:bla" , Name(_)))
//    println(parse("testbla" , Name(_)))
}




sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
        this.asInstanceOf[Obj].value.find(_._1 == s).get._2
}
case class Str(value: java.lang.String) extends AnyVal with Val
case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
case class Arr(value: Val*) extends AnyVal with Val
case class Num(value: Double) extends AnyVal with Val
case object False extends Val{
    def value = false
}
case object True extends Val{
    def value = true
}
case object Null extends Val{
    def value = null
}