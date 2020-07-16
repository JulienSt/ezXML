package jstengel.ezxml.core

import fastparse.NoWhitespace._
import fastparse.Parsed.{Failure, Success}
import fastparse._
import jstengel.ezxml.core.HelperFunctions.mergeAttributes
import jstengel.ezxml.core.SimpleWrapper.ElemWrapper

import scala.language.implicitConversions
import scala.xml._

/**
 * This is a reduced XML-parser to parse strings generated by this library.
 * The parser is based on the ground work of Li Haoyi.
 */
object ElemParser {
    
    def parseElem(string: String): Option[Elem] =  parse(string , Element(_)) match {
        case Success(value, _) => Some(value)
        case Failure(_, _, _) => None
    }
    
    def Element[_: P]: P[Elem] = P(TagHeader).flatMapX(elem => P("/>" | ">" ~/ Content ~/ ETag(elem) ).map{
        case () => elem
        case contend: Seq[Node] => elem.addChildren(contend: _*)
    })
    
    def Content[_: P]: P[Seq[Node]] = P( (CharData | Element).rep )
    
    def TagHeader[_: P]: P[Elem] = P( "<" ~ Name ~/ (WL ~ Attribute).rep.? ~ WL.? ).map{
        case ("",  label, Some(attributes)) => Elem(null, label, mergeAttributes(attributes: _*), TopScope, true, Seq(): _*)
        case (pre, label, Some(attributes)) => Elem(pre, label, mergeAttributes(attributes: _*), TopScope, true, Seq(): _*)
        case ("",  label, Some(List())) => Elem(null, label, Null, TopScope, true, Seq(): _*)
        case (pre, label, Some(List())) => Elem(pre, label, Null, TopScope, true, Seq(): _*)
    }
    def ETag[_: P](elem: Elem): P0 = {
        if (elem.prefix != null)
            P( "</" ~ elem.prefix ~ ":" ~ elem.label ~ WL.? ~ ">" )
        else
            P( "</" ~ elem.label ~ WL.? ~ ">" )
    }
    
    def Attribute[_: P]: P[Attribute] = P( Name ~ Eq ~/ AttValue ).map{
        case ("",  key, attValue) => new UnprefixedAttribute(key, attValue, Null)
        case (pre, key, attValue) => new PrefixedAttribute(pre, key, attValue, Null)
    }
    def AttValue[_: P]: P[String] = P(
        "\"" ~/ (CharQ | Reference).rep.! ~ "\"" |
        "'" ~/ (CharA | Reference).rep.! ~ "'"
    )
    
    def Name[_: P]: P[(String, String)] = P( NameStart ~ NameChar.rep ).!.map( name => {
        val splitName = name.split(":")
        if (splitName.length == 1)
            ("", splitName(0))
        else if (splitName.length == 2)
            (splitName(0), splitName(1))
        else
            throw new Exception("The Name of the Element is not correctly formatted")
    })
    def NameStart[_: P]: P0 = P( CharPred(isNameStart) ).opaque("NameStart")
    def NameChar[_: P]: P0 = P( CharPred(isNameChar) ).opaque("NameChar")
    
    def WL0[_: P]: P[Unit] = P( ScalaWhitespace.whitespace(P.current) )
    def WL[_: P]: P0 = P( NoCut(WL0) )
    
    def Char[_: P]: P0 = P( AnyChar )
    def Char1[_: P]: P0 = P( !("<" | "&") ~ Char )
    def CharQ[_: P]: P0 = P( !"\"" ~ Char1 )
    def CharA[_: P]: P0 = P( !"'" ~ Char1 )
    def CharData[_: P]: P[Text] = P( (!"{" ~ Char1 | "{{").rep(1) ).!.map(Text(_))
    
    def Reference[_: P]: P0 = P( EntityRef | CharRef )
    def EntityRef[_: P]: P0 = P( "&" ~ NameRef ~/ ";" )
    def NameRef[_: P]: P0 = P( NameStart ~ NameChar.rep )
    def CharRef[_: P]: P0 = P( "&#" ~ Num ~/ ";" | "&#x" ~ HexNum ~/ ";" )
    def Num[_: P]: P0 = P( CharIn("0-9").rep )
    def HexNum[_: P]: P0 = P( CharIn("0-9a-fA-F").rep )
    
    def Eq[_: P]: P0 = P( WL.? ~ "=" ~ WL.? )
    
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