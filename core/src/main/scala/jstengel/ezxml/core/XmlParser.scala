package jstengel.ezxml.core

import scala.language.implicitConversions
import fastparse.NoWhitespace._
import fastparse._

import scala.xml.{Attribute, Elem, MetaData, NamespaceBinding, Node, Null, PrefixedAttribute, Text, TopScope, UnprefixedAttribute}
import HelperFunctions.mergeAttributes
import SimpleWrapper.ElemWrapper

object XmlParser {
  
  def WL0[_: P]: P[Unit] = P( ScalaWhitespace.whitespace(P.current) )
  def WL[_: P]: P0 = P( NoCut(WL0) )
  
  def XmlExpr[_: P]: P0 = P( WL ~ XmlContent.rep(min = 1, sep = WL.?) )
  def XmlPattern[_: P]: P0 = P( WL ~ ElemPattern )

  def Element[_: P]: P0 = P( TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag ) )
//      .map{
//
//  }
  def TagHeader[_: P]: P0 = P( "<" ~ Name ~/ (WL ~ Attribute).rep ~ WL.? )
//  def TagHeader[_: P]: P[Elem] = P( "<" ~ Name ~/ (WL ~ Attribute).rep.? ~ WL.? ).map{
//    case (label, "", Some(attributes)) => Elem(null, label, mergeAttributes(attributes: _*), TopScope, true, Seq(): _*)
//    case (pre, label, Some(attributes)) => Elem(pre, label, mergeAttributes(attributes: _*), TopScope, true, Seq(): _*)
//    case (label, "", Some(List())) => Elem(null, label, Null, TopScope, true, Seq(): _*)
//    case (pre, label, Some(List())) => Elem(pre, label, Null, TopScope, true, Seq(): _*)
//  }
  def ETag[_: P]: P0 = P( "</" ~ Name ~ WL.? ~ ">" )
  
  def Attribute[_: P]: P0 = P( Name ~ Eq ~/ AttValue )
//  def Attribute[_: P]: P[Attribute] = P( Name ~ Eq ~/ AttValue.! ).map{
//    case (key, "", attValue) => new UnprefixedAttribute(key, attValue, Null)
//    case (pre, key, attValue) => new PrefixedAttribute(pre, key, attValue, Null)
//  }
  def Eq[_: P]: P0 = P( WL.? ~ "=" ~ WL.? )
  def AttValue[_: P]: P0 = P(
    "\"" ~/ (CharQ | Reference).rep ~ "\"" |
    "'" ~/ (CharA | Reference).rep ~ "'"
    )
//  def AttValue[_: P]: P[String] = P(
//    "\"" ~/ (CharQ | Reference) ~ "\"" |
//    "'" ~/ (CharA | Reference) ~ "'"
//    ).!

  def Content[_: P]: P0= P( (CharData | Reference | XmlContent).rep )
  def XmlContent[_: P]: P0 = P( Unparsed | CDSect | PI | Comment | Element )

  def Unparsed[_: P]: P0 = P( UnpStart ~/ UnpData ~ UnpEnd )
  def UnpStart[_: P]: P0 = P( "<xml:unparsed" ~/ (WL ~ Attribute).rep ~ WL.? ~ ">" )
  def UnpEnd[_: P]: P0 = P( "</xml:unparsed>" )
  def UnpData[_: P]: P0 = P( (!UnpEnd ~ AnyChar).rep )

  def CDSect[_: P]: P0 = P( CDStart ~/ CData ~ CDEnd )
  def CDStart[_: P]: P0 = P( "<![CDATA[" )
  def CData[_: P]: P0 = P( (!"]]>" ~ Char).rep )
  def CDEnd[_: P]: P0 = P( "]]>" )

  def Comment[_: P]: P0 = P( "<!--" ~/ ComText ~ "-->" )
  def ComText[_: P]: P0 = P( (!"--" ~ Char).rep ~ ("-" ~ &("--")).? )

  def PI[_: P]: P0 = P( "<?" ~ PITarget ~ PIProcText.? ~ "?>" )
  def PITarget[_: P]: P0 = P( !(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name )
  def PIProcText[_: P]: P0 = P( WL ~ (!"?>" ~ Char).rep )

  def Reference[_: P]: P0 = P( EntityRef | CharRef )
  def EntityRef[_: P]: P0 = P( "&" ~ Name ~/ ";" )
  def CharRef[_: P]: P0 = P( "&#" ~ Num ~/ ";" | "&#x" ~ HexNum ~/ ";" )
  def Num[_: P]: P0 = P( CharIn("0-9").rep )
  def HexNum[_: P]: P0 = P( CharIn("0-9a-fA-F").rep )

  def CharData[_: P]: P0 = P( (!"{" ~ Char1 | "{{").rep(1) )

  def Char[_: P]: P0 = P( AnyChar )
  def Char1[_: P]: P0 = P( !("<" | "&") ~ Char )
  def CharQ[_: P]: P0 = P( !"\"" ~ Char1 )
  def CharA[_: P]: P0 = P( !"'" ~ Char1 )

  def Name[_: P]: P0 = P( PrefixedName | UnPrefixedName )
  def PrefixedName[_: P]: P0 = P( (NameStart ~ NameChar.rep).! ~ ":" ~ NameChar.rep.! )
  def UnPrefixedName[_: P]: P0 = P( NameStart ~ NameChar.rep )
//  def Name[_: P]: P[(String, String)] = P( (NameStart ~ NameChar.rep).! ~ ":".? ~ NameChar.rep.! )
  def NameStart[_: P]: P0 = P( CharPred(isNameStart) ).opaque("NameStart")
  def NameChar[_: P]: P0 = P( CharPred(isNameChar) ).opaque("NameChar")

  def ElemPattern[_: P]: P0 = P( TagPHeader ~/ ("/>" | ">" ~/ ContentP ~/ ETag ) )
//  def ElemPattern[_: P]: P[Elem] = P( TagPHeader ~/ ( "/>" | ">" ~/ ContentP ~/ ETag ).map{
//    case s: Seq[Node] => Some(s)
//    case _ => None
//  }).map{
//    case (elem, Some(nodes)) => elem.addChildren(nodes: _*)
//    case (elem, None) => elem
//  }
  def TagPHeader[_: P]: P0 = P( "<" ~ Name ~ WL.?  )
//  def TagPHeader[_: P]: P[Elem] = P( "<" ~ Name ~ WL.?  ).map{
//    case (label, "") => Elem(null, label, Null, TopScope, true, Seq(): _*)
//    case (pre, label) => Elem(pre, label, Null, TopScope, true, Seq(): _*)
//  }

  def ContentP[_: P]: P0  = P( ( CharDataP | ElemPattern ).rep )
//  def ContentP[_: P]: P[Seq[Node]] = P( ( CharDataP | ElemPattern ).rep )
  def CharDataP[_: P]: P0 = P( "&" ~ CharData.? | CharData ) // matches weirdness of scalac parser on xml reference.
//  def CharDataP[_: P]: P[Text] = P( "&" ~ CharData.? | CharData ).!.map(Text(_)) // matches weirdness of scalac parser on xml reference.

  //================================================================================
  // From `scala.xml.parsing.TokenTests`
  //================================================================================

  /**
   * {{{
   *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *             | CombiningChar | Extender
   *  }}}
   *  See [4] and Appendix B of XML 1.0 specification.
   */
  def isNameChar(ch: Char): Boolean = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.

    isNameStart(ch) || (getType(ch).toByte match {
      case COMBINING_SPACING_MARK | ENCLOSING_MARK | NON_SPACING_MARK | MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
      case _ => ".-:" contains ch
//      case _ => ".-" contains ch
    })
  }

  /**
   * {{{
   *  NameStart ::= ( Letter | '_' )
   *  }}}
   *  where Letter means in one of the Unicode general
   *  categories `{ Ll, Lu, Lo, Lt, Nl }`.
   *
   *  We do not allow a name to start with `:`.
   *  See [3] and Appendix B of XML 1.0 specification
   */
  def isNameStart(ch: Char) : Boolean = {
    import java.lang.Character._
    
    getType(ch).toByte match {
      case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER | LETTER_NUMBER => true
      case _                                => ch == '_'
    }
  }
}
