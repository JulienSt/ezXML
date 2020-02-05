package indv.jstengel.ezxml.extension.rt


import scala.language.reflectiveCalls
import scala.reflect.ClassTag
import scala.xml.{Attribute, Elem, Null, Text, TopScope}
import indv.jstengel.ezxml.core.SimpleWrapper.ElemWrapper
import RuntimeReflectHelper.{arrayType,
    asArrayType,
    classTagOf,
    createStringRepresentation,
    getType,
    getTypeFromString,
    getTypeParams,
    isSimpleType,
    iterableType,
    tagOf
}
import indv.jstengel.ezxml.extension.mapping.FieldMappings


// https://medium.com/@sinisalouc/overcoming-type-erasure-in-scala-8f2422070d20
object RTConverter {

    import scala.reflect.{runtime => rt}
    import rt.{universe => ru}
    import ru._
    private implicit val rm: Mirror = rt.currentMirror
    
    type XmlCapable = { def saveAsXml: Elem }
    
    def convertToXML[A] (a        : A,
                         mappings : FieldMappings = FieldMappings(),
                         pre      : String = null)
                        (implicit tt : TypeTag[A], ct : ClassTag[A]) : Elem = {
        try {
            /* sadly */
            val Elem(_, l, att, s, c @ _*) = a.asInstanceOf[XmlCapable].saveAsXml
            Elem(pre, l, att, s, true, c: _*)
        }
        catch { case _ : NoSuchMethodException =>
            val ttType    = getType(a)
            val typeParams = getTypeParams(ttType)
            val className = createStringRepresentation(ttType)(typeParams)
            
            if (isSimpleType(ttType)) {
                Elem(pre, className, Null, TopScope, true, Seq(): _*) % Attribute("value", Text(a.toString), Null)
            }
            
            else if (ttType <:< arrayType) {
                val tParams = getTypeParams(tt.tpe)
                val tParam = tParams.head
                val tagParam = tagOf(tParam)
                val arr = asArrayType(a)(tagParam, classTagOf(tParam)).map { e =>
                    convertToXML(e, mappings)(tagParam, classTagOf(tParam))
                }.toIndexedSeq
                Elem(pre, createStringRepresentation(ttType)(tParams), Null, TopScope, true, arr: _*)
            }
            
            else if (ttType <:< iterableType) {
                val iterator = a.asInstanceOf[IterableOnce[Any]].iterator
                val arrayToElemSeq: (Any => Elem) => Seq[Elem] = aToXml => iterator.map(aToXml).toSeq
                val seq = typeParams match {
                    case Nil              => arrayToElemSeq(e => convertToXML(e, mappings))
                    case typeParam :: Nil =>
                        arrayToElemSeq(e => convertToXML(e, mappings)(tagOf(typeParam), ClassTag(e.getClass)))
                    case paramList =>
                        val n = paramList.length
                        val parameterizedTupleName = s"scala.Tuple$n" + className.dropWhile(_ != '[')
                        val tupleType = getTypeFromString(parameterizedTupleName)
                        arrayToElemSeq(e => convertToXML(e, mappings)(tagOf(tupleType), ClassTag(e.getClass)))
                }
                Elem(pre, className, Null, TopScope, true, seq: _*)
            }
            
            else {
                val reflectedObj  = rm.reflect(a)
                val reflectedType = reflectedObj.symbol.toType
                reflectedType.members
                             .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                             .get
                             .paramLists
                             .flatten
                             .foldLeft(Elem(pre, className, Null, TopScope, true, Seq() : _*)){
                                 case (elem @ Elem(pre, l, att, s, c @ _*), p) =>
                                     val memberName  = p.name
                                     val memberNameStr = memberName.toString
                                     val member = reflectedType.member(
                                         TermName(mappings.getSubstituteName(memberNameStr)(tagOf(reflectedType)))
                                     )
                                     
                                     val fieldValue  =
                                         try
                                             reflectedObj.reflectField(member.asTerm).get
                                         catch {
                                             case _: ScalaReflectionException =>
                                                 reflectedObj.reflectMethod(member.asMethod).apply()
                                         }
                                     
                                     val memberType =
                                         try
                                             rm.reflect(fieldValue).symbol.toType
                                         catch {
                                             case _ : ScalaReflectionException => member.typeSignature.resultType
                                         }
                                     
                                     val memberTypeName = memberType.typeSymbol.fullName
                                     
                                     if(fieldValue == null)
                                         elem addAttWithPre (memberTypeName, memberNameStr, "_NULL_")
                                     else if (isSimpleType(memberType))
                                         elem addAttWithPre (memberTypeName, memberNameStr, fieldValue.toString)
                                     else {
                                         val fieldType = try           /* otherwise you get [=> A] for [A] */
                                                              member.asMethod.returnType
                                                          catch {
                                                              case _ : ScalaReflectionException => member.typeSignature
                                                          }
                                         Elem(pre, l, att, s, false, c ++
                                             convertToXML(fieldValue, mappings, memberNameStr)
                                                         (tagOf(fieldType), ClassTag(fieldValue.getClass)) : _*)
                                     }
                             }
            }
        }
    }
    
}
