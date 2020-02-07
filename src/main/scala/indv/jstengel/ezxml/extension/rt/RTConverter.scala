package indv.jstengel.ezxml.extension.rt


import scala.language.reflectiveCalls
import scala.reflect.ClassTag
import scala.xml.{Attribute, Elem, Null, Text, TopScope}
import indv.jstengel.ezxml.core.SimpleWrapper.ElemWrapper
import RuntimeReflectHelper.{
    NullFlag,
    arrayType,
    asArrayType,
    classTagOf,
    createStringRepresentation,
    getType,
    getTypeFromString,
    getTypeParams,
    isSimpleType,
    iterableType,
    tagOf,
    isConstructorMissing
}
import indv.jstengel.ezxml.extension.mapping.FieldMapping.FieldMappings


// https://medium.com/@sinisalouc/overcoming-type-erasure-in-scala-8f2422070d20
object RTConverter {

    import scala.reflect.{runtime => rt}
    import rt.{universe => ru}
    import ru._
    private implicit val rm: Mirror = rt.currentMirror
    
    type XmlCapable = { def saveAsXml: Elem }
    
    private[extension]
    def convertToXML[A] (a           : A,
                         mappings    : FieldMappings = Seq(),
                         pre         : String = null,
                         isRecursive : Boolean = false)
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
            
            else if ((ttType <:< iterableType || className.startsWith("scala.collection.")) &&
                     !isRecursive &&
                     (isConstructorMissing(ttType) || ttType.typeSymbol.isAbstract)) {
                val iterator = a.asInstanceOf[IterableOnce[Any]].iterator
                val arrayToElemSeq: (Any => Elem) => Seq[Elem] = aToXml => iterator.map(aToXml).toSeq
                val seq = typeParams match {
                    case Nil              =>
                        val asList = List.from(iterator)
                        val listHead = asList.headOption
                        val isIterableRecursive = listHead.contains(a)
                        if (isIterableRecursive)
                            asList.map(e => convertToXML(e, mappings, isRecursive = isIterableRecursive))
                        else if (listHead.nonEmpty)
                            asList.map(e => convertToXML(e, mappings))
                        else
                            Nil
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
                val reflectedType = reflectedObj.symbol.typeSignature
                val constructor = reflectedType.members
                                               .collectFirst{ case m : MethodSymbol if m.isPrimaryConstructor => m }
                                               .get
                constructor
                    .paramLists
                    .flatten
                    .foldLeft(Elem(pre, className, Null, TopScope, true, Seq() : _*)){
                        case (elem @ Elem(pre, l, att, s, c @ _*), p) =>
                            val fieldName  = p.name
                            val fieldNameStr = fieldName.toString
                            
                            val member = reflectedType.member(
                                TermName(mappings.getSubstituteName(fieldNameStr)(tagOf(constructor.returnType)))
                            )
                                     
                            val fieldValue  = try
                                                  reflectedObj.reflectField(member.asTerm).get
                                              catch {
                                                  case _: ScalaReflectionException => try {
                                                      reflectedObj.reflectMethod(member.asMethod).apply()
                                                  } catch {
                                                      case r: ScalaReflectionException =>
                                                          val explanation = ScalaReflectionException(
                                                              "A field couldn't be accessed through reflection."+
                                                              "\nMake sure every Field you want to access " +
                                                              "is actually publicly accessible.\n" +
                                                              "If You don't want to change the privacy of a field,\n" +
                                                              "look into indv.jstengel.ezxml.extension" +
                                                              ".mapping.FieldMapping.FieldMappings")
                                                          explanation.setStackTrace(Array.concat(
                                                              explanation.getStackTrace,
                                                              r.getStackTrace).drop(6)
                                                          )
                                                          throw explanation
                                                  }
                                              }
                                     
                            val valueType = try
                                                rm.reflect(fieldValue).symbol.toType
                                            catch {
                                                case _ : ScalaReflectionException => member.typeSignature.resultType
                                                case _ : NullPointerException     => member.typeSignature.resultType
                                            }
                                     
                            val valueTypeName = valueType.typeSymbol.fullName
    
                            val fieldType = try
                                                member.asMethod.returnType /* otherwise you get [=> A] for [A] */
                                            catch {
                                                case _ : ScalaReflectionException => member.typeSignature
                                            }
    
                            val newLabel = if (fieldType.typeSymbol.isAbstract && l.contains(fieldType.toString))
                                               l.replace(fieldType.typeSymbol.fullName, valueTypeName)
                                           else
                                               l
                                     
                            if(fieldValue == null)
                                elem addAttWithPre (valueTypeName, fieldNameStr, NullFlag)
                            else if (isSimpleType(valueType)) {
                                Elem(pre, newLabel, att, s, false, c:_*).addAttWithPre(valueTypeName,
                                                                                       fieldNameStr,
                                                                                       fieldValue.toString)
                            } else {
                                val savedType = if (!fieldType.typeSymbol.isAbstract)
                                                    fieldType
                                                else
                                                    valueType
                                Elem(pre, newLabel, att, s, false, c ++
                                                                   convertToXML(fieldValue, mappings, fieldNameStr)
                                                                               (tagOf(savedType),
                                                                                ClassTag(fieldValue.getClass)) : _*)
                            }
                    }
            }
        }
    }
    
}
