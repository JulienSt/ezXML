package indv.jstengel.ezxml.extension.reflection


import scala.language.reflectiveCalls
import scala.reflect.{ClassTag, api}
import scala.xml.{Attribute, Elem, Null, Text, TopScope}
import indv.jstengel.ezxml.core.SimpleWrapper.ElemWrapper
import RuntimeReflectHelper.{arrayType,
    asArrayType,
    classTagOf,
    isSimpleType,
    createStringRepresentation,
    getType,
    getTypeParams,
    iterableType,
    getTypeFromString,
    tagOf}


// https://medium.com/@sinisalouc/overcoming-type-erasure-in-scala-8f2422070d20
object RTConverter {

    import scala.reflect.{runtime => rt}
    import rt.{universe => ru}
    import ru._
    private implicit val rm: Mirror = rt.currentMirror
    
    type XmlCapable = { def saveAsXml: Elem }
    
    // fieldNameMap
    // classNameMap
    // accessNameMap
    def convertToXML[A] (a : A,
                         mapFieldNames: (String, String) => Option[String] = (_, _) => None,
                         pre : String = null)
                        (implicit tt : TypeTag[A], ct : ClassTag[A]) : Elem = {
        try {
            val Elem(_, l, att, s, c @ _*) = a.asInstanceOf[XmlCapable].saveAsXml
            println("fone")
            Elem(pre, l, att, s, true, c: _*)
        }
        catch { case _ : Throwable =>
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
                    convertToXML(e, mapFieldNames)(tagParam, classTagOf(tParam))
                }.toIndexedSeq
                Elem(pre, createStringRepresentation(ttType)(tParams), Null, TopScope, true, arr: _*)
            }
            
            else if (ttType <:< iterableType) {
                val iterator = a.asInstanceOf[IterableOnce[Any]].iterator
                val arrayToElemSeq: (Any => Elem) => Seq[Elem] = aToXml => iterator.map(aToXml).toSeq
                val seq = typeParams match {
                    case Nil              => arrayToElemSeq(e => convertToXML(e, mapFieldNames))
                    case typeParam :: Nil =>
                        arrayToElemSeq(e => convertToXML(e, mapFieldNames)(tagOf(typeParam), ClassTag(e.getClass)))
                    case paramList =>
                        val n = paramList.length
                        val parameterizedTupleName = s"scala.Tuple$n" + className.dropWhile(_ != '[')
                        val tupleType = getTypeFromString(parameterizedTupleName)
                        arrayToElemSeq(e => convertToXML(e, mapFieldNames)(tagOf(tupleType), ClassTag(e.getClass)))
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
                                         mapFieldNames(className, memberNameStr) match {
                                             case Some(alternative) => TermName(alternative)
                                             case None => memberName
                                         }
                                     )
                                     
                                     val fieldValue  =
                                         try
                                             reflectedObj.reflectField(member.asTerm).get
                                         catch {
                                             case _: ScalaReflectionException =>
                                                 reflectedObj.reflectMethod(member.asMethod).apply()
                                         }

                                     // todo hier getTypeFromValue hier einsetzen
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
                                             convertToXML(fieldValue, mapFieldNames, memberNameStr)
                                                         (tagOf(fieldType), ClassTag(fieldValue.getClass)) : _*)
                                     }
                             }
            }
        }
    }
    
//    val fieldValue  = try
//        reflectedObj.reflectField(member.asTerm).get
//    catch {
//        case _: ScalaReflectionException =>
//            reflectedObj.reflectMethod(member.asMethod).apply()
//    }
//
//    val memberType = try           /* otherwise you get [=> A] for [A] */
//        member.asMethod.returnType
//    catch {
//        case _ : ScalaReflectionException => member.typeSignature
//    }
//    val memberClassName = fieldValue.getClass.getSimpleName
//
//    if(fieldValue == null)
//        elem % Attribute(memberNameStr, memberClassName, Text("_NULL_"), Null)
//    else if (isSimple(fieldValue))
//             elem % Attribute(memberNameStr, memberClassName, Text(fieldValue.toString), Null)
//    else {
//        Elem(pre,
//             l,
//             att,
//             s,
//             false,
//             c ++ convertToXML(fieldValue,
//                               mapFieldNames,
//                               memberNameStr)(tagOf(memberType),
//                                              ClassTag(fieldValue.getClass)) : _*)

//    def isSimple(a: Any): Boolean = {
//        val c = a.getClass
//        val cName = Option(c.getCanonicalName).getOrElse(c.getName)
//        cName.contains("java.lang") && !cName.contains("[]")
//    }
    
}
